"""Graph state threaded through the cohort / analysis / judge nodes."""

from __future__ import annotations

from typing import Any, TypedDict


class GraphState(TypedDict, total=False):
    """Mutable state for the LangGraph run (total=False: nodes fill keys as they go)."""

    # --- inputs (set once by entry.generate_yaml) ---
    prompt: str
    catalog: dict[str, dict[str, Any]]
    rdv_dict: dict[str, dict[str, Any]]
    dataset: str
    creator: str
    img: str

    # --- triage gate ---
    triage: dict[str, Any]      # {"is_app_request": bool, "is_sufficient": bool, "reason": str}

    # --- agent outputs (one slice each) ---
    cohort: dict[str, Any]      # {"initialCohorts": [...]}
    analysis: dict[str, Any]    # {"title": ..., "description": ..., "analysis": [...]}

    # --- judge / retry bookkeeping ---
    errors: list                # list[JudgeError] from the last judge pass
    round: int                  # retry counter (0 on first pass; cap at 2)

    # --- final output (set by the assemble node) ---
    yaml: str                   # the assembled PICTURE YAML string
