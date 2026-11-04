"""Deterministic validation of the merged YAML slices."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from langgraph.graph import END

from .log import get_logger
from .state import GraphState

MAX_ROUNDS = 2

logger = get_logger(__name__)

@dataclass
class JudgeError:
    """A single validation failure, tagged with the slice that must be re-run."""

    slice: str   # "cohort" | "analysis"
    message: str


def _val_ok(input_type: str, val: Any) -> bool:
    """True if `val` matches the shape implied by the column's input_type."""
    if input_type == 'text':
        return isinstance(val, str)
    if input_type == 'select':
        return isinstance(val, str) or (
            isinstance(val, list) and all(isinstance(x, str) for x in val))
    if input_type == 'date_range':
        return isinstance(val, list) and len(val) == 2 and all(isinstance(x, str) for x in val)
    if input_type in ('age_range', 'numeric_range'):
        return (isinstance(val, list) and len(val) == 2
                and all(isinstance(x, (int, float)) and not isinstance(x, bool) for x in val))
    return True

def check_column_names(obj: dict[str, Any],
                       rdv_dict: dict[str, dict[str, Any]]) -> list[JudgeError]:
    """Flag cohort val shapes that don't match input_type, and params that name unknown columns.

    Schema enforces col-in-rdv and param-key validity; this only checks what the schema can't.
    """
    col_input_type: dict[tuple[str, str], str] = {}
    all_col_names: set[str] = set()
    for slug, info in rdv_dict.items():
        for col in info.get("columns") or []:
            name = col.get("name")
            if name:
                all_col_names.add(name)
                if col.get("input_type"):
                    col_input_type[(slug, name)] = col["input_type"]

    # No column metadata -> skip rather than flag everything (crash/false-positive safety).
    if not all_col_names:
        return []

    errors: list[JudgeError] = []

    for cohort in obj.get("initialCohorts") or []:
        for filt in cohort.get("config") or []:
            input_type = col_input_type.get((filt.get("rdv"), filt.get("column")))
            if input_type and not _val_ok(input_type, filt.get("val")):
                errors.append(JudgeError(
                    "cohort", f"val {filt.get('val')!r} is not valid for "
                              f"{filt.get('column')!r} (a {input_type} column)"))

    # cohort_col/stratify_by must be "cohort", not a data column — a data column crashes the analysis.
    stratify_keys = {"cohort_col", "stratify_by"}

    for ana in obj.get("analysis") or []:
        for method in ana.get("methodList") or []:
            for k, v in (method.get("params") or {}).items():
                if k in stratify_keys and isinstance(v, str) and v and v != "cohort":
                    errors.append(JudgeError(
                        "analysis", f"param {k}={v!r} must be \"cohort\" (the cohort-stratification "
                                    f"column), not a data column"))
                    continue
                names_a_column = k == "col" or k.endswith("_col") or k == "stratify_by"
                if (names_a_column and isinstance(v, str) and v
                        and v != "cohort" and v not in all_col_names):
                    errors.append(JudgeError(
                        "analysis", f"param {k}={v!r} is not a known column name"))

    return errors
    
    


def validate(obj: dict[str, Any],
             rdv_dict: dict[str, dict[str, Any]]) -> list[JudgeError]:
    """Run every deterministic check and return the combined, slice-tagged error list."""
    return check_column_names(obj, rdv_dict)


def judge_node(state: GraphState) -> dict:
    """LangGraph node: merge slices, run validate(), return {'errors': [...]} into state."""
    cohort = state.get('cohort') or {}
    analysis = state.get('analysis') or {}
    round_ = state.get('round', 0) + 1

    errors: list[JudgeError] = []
    if cohort.get('_failed'):
        errors.append(JudgeError('cohort', 'LLM agent failed to produce valid output after all retries'))
    if analysis.get('_failed'):
        errors.append(JudgeError('analysis', 'LLM agent failed to produce valid output after all retries'))

    if not errors:
        obj = {
            'initialCohorts': cohort.get('initialCohorts', []),
            'analysis':       analysis.get('analysis', [])
        }
        errors = validate(obj, state['rdv_dict'])

    if errors:
        logger.info('judge round %d: %d validation error(s)', round_, len(errors))
        for e in errors:
            logger.info('  - [%s] %s', e.slice, e.message)
    else:
        logger.info('judge round %d: passed (no errors)', round_)

    return {'errors': errors, 'round': round_}


def route_after_judge(state: GraphState) -> list[str]:
    """Conditional edge: re-run failing slices, assemble when clean, or END when budget exhausted."""
    errors = state.get('errors') or []
    if not errors:
        return ['assemble']
    if state.get('round', 0) >= MAX_ROUNDS:
        return [END]
    slices = {error.slice for error in errors}
    return [s for s in ('cohort', 'analysis') if s in slices]
            
