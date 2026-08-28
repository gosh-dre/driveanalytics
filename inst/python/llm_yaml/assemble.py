"""Merge the agent slices and serialise the final PICTURE YAML."""

from __future__ import annotations

from typing import Any

import yaml

from .log import get_logger
from .state import GraphState

logger = get_logger(__name__)


def _strip_nulls(x: Any) -> Any:
    """Drop None values recursively; PICTURE's loader expects optional keys omitted."""
    if isinstance(x, dict):
        return {k: _strip_nulls(v) for k, v in x.items() if v is not None}
    if isinstance(x, list):
        return [_strip_nulls(v) for v in x]
    return x


def merge_slices(state: GraphState) -> dict:
    """Combine cohort + analysis slices with premade fields (creator/dataset/img/outputs)."""
    cohort = state.get('cohort', {})
    analysis = state.get('analysis', {})

    merged = {
        'title':              analysis.get('title', ''),
        'description':        analysis.get('description', ''),
        'creator':            state.get('creator', ''),
        'img':                state.get('img', ''),
        'dataset':            state.get('dataset', ''),
        # 'amount_of_data_to_load': 'limited' not implemented
        'initialCohorts':     cohort.get('initialCohorts', {}),
        'offerCohortBuilder': True,
        'analysis':           analysis.get('analysis', {}),
        #'outputs': {
        #    'interactive': True,
        #    'pdf': False
        #}
    }
    return merged


def assemble_yaml(state: GraphState) -> dict:
    """Serialise the merged state to YAML and return {'yaml': ...}."""
    merged = merge_slices(state)
    cleaned = _strip_nulls(merged)
    dump = yaml.safe_dump(cleaned, sort_keys=False, default_flow_style=False,
                          allow_unicode=True, width=120)
    logger.info('assembled final YAML (%d chars, %d cohort(s), %d tab(s))', len(dump),
                len(cleaned.get('initialCohorts') or []), len(cleaned.get('analysis') or []))
    return {'yaml': dump}
