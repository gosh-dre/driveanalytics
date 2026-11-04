"""Reticulate entry point — the single function R calls."""

from __future__ import annotations

from typing import Any, Callable, Optional

from openai import BadRequestError

from .errors import PicturePicLLMError
from .graph import build_graph
from .judge import MAX_ROUNDS
from .log import configure_logging, drain_thread_logs, get_logger
from .state import GraphState

logger = get_logger(__name__)


def _report(progress: Optional[Callable[[str, float], None]], stage: str, frac: float) -> None:
    """Call the R progress callback if one was supplied; never let it break generation."""
    if progress is None:
        return
    try:
        progress(stage, frac)
    except Exception:
        # Progress reporting is cosmetic — a failing callback must not abort the pipeline.
        pass


def _r_log(log_event: Optional[Callable[[str, str], None]], level: str, message: str) -> None:
    """Forward one log event to the R callback; safe only from the main thread (parallel nodes must not call R directly)."""
    if log_event is None:
        return
    try:
        log_event(level, message)
    except Exception:
        pass


def _error_slices(errors: Any) -> set[str]:
    """Pull the slice tags ('cohort'/'analysis') out of a judge error list, tolerating dicts."""
    slices: set[str] = set()
    for e in errors or []:
        slice_ = getattr(e, 'slice', None)
        if slice_ is None and isinstance(e, dict):
            slice_ = e.get('slice')
        if slice_:
            slices.add(slice_)
    return slices


def _error_messages(errors: Any) -> list[str]:
    """Pull the human messages out of a judge error list, tolerating dicts."""
    msgs: list[str] = []
    for e in errors or []:
        msg = getattr(e, 'message', None)
        if msg is None and isinstance(e, dict):
            msg = e.get('message')
        if msg:
            msgs.append(msg)
    return msgs


def _stage_after(chunk: dict[str, Any]) -> Optional[tuple[str, float]]:
    """Map a finished-node chunk to (stage, fraction) for progress reporting, or None if not tracked."""
    if 'assemble' in chunk:
        return ('finalise', 0.95)

    if 'judge' in chunk:
        update = chunk['judge'] or {}
        errors = update.get('errors') or []
        if not errors or update.get('round', 0) >= MAX_ROUNDS:
            return ('validate', 0.6)
        slices = _error_slices(errors)
        if slices == {'cohort'}:
            return ('refine_cohort', 0.55)
        if slices == {'analysis'}:
            return ('refine_analysis', 0.55)
        return ('refine', 0.55)

    if 'triage' in chunk:
        return ('agents', 0.1)
    if 'cohort' in chunk:
        return ('cohort', 0.3)
    if 'analysis' in chunk:
        return ('analysis', 0.4)
    return None


def generate_yaml(prompt: str,
                  catalog: dict[str, dict[str, Any]],
                  rdv_dict: dict[str, dict[str, Any]],
                  dataset: str = "dmv",
                  creator: Optional[str] = None,
                  img: Optional[str] = None,
                  progress: Optional[Callable[[str, float], None]] = None,
                  log_event: Optional[Callable[[str, str], None]] = None) -> Any:
    """Run the LangGraph pipeline; return YAML on success or {'refused': True, 'reason': ...} on rejection.

    Raises PicturePicLLMError on failure. Optional callbacks: `progress(stage, fraction)` and
    `log_event(level, message)` where level is 'INFO'/'WARN'/'ERROR'.
    """
    configure_logging(log_event=log_event)

    state = GraphState(
        prompt=prompt,
        catalog=catalog,
        rdv_dict=rdv_dict,
        dataset=dataset,
        creator=creator or '',
        img=img or ''
    )

    logger.info('generate_yaml: start (dataset=%s, creator=%r, prompt_chars=%d)',
                dataset, creator or '', len(prompt or ''))
    _r_log(log_event, 'INFO', f'pipeline started (dataset={dataset}, prompt {len(prompt or "")} chars)')

    graph = build_graph()
    frac = 0.05
    _report(progress, 'triage', frac)

    yaml_out: Optional[str] = None
    triage: dict[str, Any] = {}
    residual_errors: Any = []
    try:
        for chunk in graph.stream(state, stream_mode="updates"):
            drain_thread_logs()
            if 'triage' in chunk:
                triage = (chunk['triage'] or {}).get('triage') or {}
                _r_log(log_event, 'INFO',
                       f"triage: is_app_request={triage.get('is_app_request')}, "
                       f"is_sufficient={triage.get('is_sufficient')}, "
                       f"reason={triage.get('reason')!r}")
            
            if 'cohort' in chunk:
                cohort = (chunk['cohort'] or {}).get('cohort') or {}
                n = len(cohort.get('initialCohorts') or [])
                _r_log(log_event, 'INFO', f'cohort agent returned {n} cohort(s)')
            if 'analysis' in chunk:
                ana = (chunk['analysis'] or {}).get('analysis') or {}
                _r_log(log_event, 'INFO',
                       f"analysis agent returned {len(ana.get('analysis') or [])} tab(s)")
            if 'judge' in chunk:
                residual_errors = (chunk['judge'] or {}).get('errors') or []
                round_ = (chunk['judge'] or {}).get('round')
                msg = f'judge round {round_}: {len(residual_errors)} error(s)'
                if residual_errors:
                    msg += '; ' + '; '.join(_error_messages(residual_errors))
                _r_log(log_event, 'WARN' if residual_errors else 'INFO', msg)
            if 'assemble' in chunk:
                yaml_out = (chunk['assemble'] or {}).get('yaml')
                _r_log(log_event, 'INFO', f'assembled YAML ({len(yaml_out or "")} chars)')
            stage = _stage_after(chunk)
            if stage is not None:
                key, base = stage
                # Keep the bar monotonic across retry rounds; the stage text conveys the rerun.
                frac = max(frac, base)
                _report(progress, key, frac)
    except BadRequestError as e:
        # Azure/Portkey content filter blocked the request at the API level.
        code = getattr(e, 'code', None) or ''
        
        if 'content_filter' in str(code).lower() or 'content_filter' in str(e).lower():
            reason = 'Request blocked by content filter.'
            logger.info('refused by content filter: %s', e)
            _r_log(log_event, 'WARN', 'refused by content filter')
            return {'refused': True, 'reason': reason}
        
        # Any other BadRequestError (bad params, etc.) is a real failure.
        logger.exception('pipeline failed: bad request')
        _r_log(log_event, 'ERROR', f'pipeline failed: {e}')
        raise PicturePicLLMError(f'YAML generation failed; {e}') from e
    except Exception as e:
        logger.exception('pipeline failed during generation')
        _r_log(log_event, 'ERROR', f'pipeline failed: {e}')
        raise PicturePicLLMError(f'YAML generation failed; {e}') from e
    finally:
        drain_thread_logs()

    # Only is_app_request is a hard gate
    if triage and not triage.get('is_app_request'):
        reason = triage.get('reason') or 'This request was not accepted.'
        logger.info('refused by triage gate: %s', reason)
        _r_log(log_event, 'WARN', f'refused by triage gate: {reason}')
        return {'refused': True, 'reason': reason}

    if residual_errors:
        msg = 'generated app failed validation: ' + '; '.join(_error_messages(residual_errors))
        logger.error(msg)
        _r_log(log_event, 'ERROR', msg)
        raise PicturePicLLMError(msg)

    if not yaml_out:
        logger.error('pipeline produced no YAML')
        _r_log(log_event, 'ERROR', 'pipeline produced no YAML')
        raise PicturePicLLMError('pipeline produced no YAML')

    logger.info('generate_yaml: success (%d chars)', len(yaml_out))
    _r_log(log_event, 'INFO', f'pipeline finished: YAML generated ({len(yaml_out)} chars)')
    return yaml_out
