"""The LLM nodes: triage_agent, cohort_agent and analysis_agent."""

from __future__ import annotations

import os
from pathlib import Path

import yaml

from langchain_core.messages import HumanMessage, SystemMessage

from .llm import make_chat_model
from .log import get_logger
from .models import TriageVerdict, build_analysis_output_model, build_cohort_output_model
from .state import GraphState

_PROMPTS = Path(__file__).parent / 'prompts'

logger = get_logger(__name__)

def _structured_output_kwargs() -> dict:
    """Return with_structured_output kwargs for the active backend.

    HuggingFace: json_mode (no schema support). All others: json_schema strict.
    """
    if os.getenv('LLM_BACKEND', 'portkey').lower() == 'huggingface':
        return {'method': 'json_mode'}
    return {'method': 'json_schema', 'strict': True}


def _format_rdvs(rdv_dict: dict) -> str:
    """Render rdv_dict as readable 'rdv: col_1, col_2, ...' lines for the prompt."""
    lines = []
    for slug, info in rdv_dict.items():
        lines.append(f'- {slug}:')
        for c in info.get('columns') or []:
            lines.append(f"    - {c['name']} ({c['input_type']}): {c['label']}")
    return '\n'.join(lines)


def _format_sources(rdv_dict: dict) -> str:
    """Render each source under its df_ handle so df_ params and *_col params share one listing."""
    lines = []
    for slug, info in rdv_dict.items():
        lines.append(f'- df_{slug}:')
        for c in info.get('columns') or []:
            lines.append(f"    - {c['name']} ({c['input_type']}): {c['label']}")
    return '\n'.join(lines)


def _format_catalog(catalog: dict) -> str:
    """Render each module with its rpkg and params, one param per line."""
    lines = []
    for slug, info in catalog.items():
        lines.append(f"- {slug} ({info.get('rpkg')}):")
        for param in info.get('params') or []:
            lines.append(f'    - {param}')
    return '\n'.join(lines)


def _format_feedback(state: GraphState, slice_: str) -> str:
    """Render this slice's judge errors as a 'fix these' block, or '' on the first pass."""
    msgs = [e.message for e in (state.get('errors') or []) if e.slice == slice_]
    if not msgs:
        return ''
    return 'PROBLEMS WITH THE PREVIOUS ATTEMPT (fix all of these):\n- ' + '\n- '.join(msgs)


def _build_cohort_prompt(state: GraphState) -> str:
    """Assemble the cohort agent's system prompt: rules + rdv_dict slice + any judge feedback."""
    template = yaml.safe_load((_PROMPTS / 'cohort_prompt.yaml').read_text())['template']
    return template.format(
        available_data=_format_rdvs(state['rdv_dict']),
        user_request=state['prompt'],
        feedback=_format_feedback(state, 'cohort')
    )


def _derive_query_types(data: dict, rdv_dict: dict) -> dict:
    """Fill each filter's query_type from its column's filter_type (premade, not LLM'd)."""
    filter_type = {
        (slug, col['name']): col['filter_type']
        for slug, info in rdv_dict.items()
        for col in info.get('columns', []) or []
    }
    for cohort in data.get('initialCohorts', []) or []:
        for filt in cohort.get('config', []) or []:
            filt['type'] = 'filter'  # injected post-hoc; excluded from schema so LLM never outputs it
            filt['query_type'] = filter_type.get((filt.get('rdv'), filt.get('column')))
    return data


def _invoke_structured(structured, prompt, attempts: int = 3):
    """Invoke a structured runnable, retrying up to `attempts` times on malformed output.

    Temperature=1 occasionally causes schema parse failures; retries resample.
    """
    last_error = None
    for attempt in range(1, attempts + 1):
        try:
            return structured.invoke(prompt)
        except Exception as error:
            last_error = error
            logger.warning('structured output attempt %d/%d failed: %s', attempt, attempts, error)
    logger.error('structured output failed after %d attempt(s)', attempts)
    raise last_error


def triage_agent(state: GraphState) -> dict:
    """LangGraph node: classify the request; return {'triage': ...}.

    Prompt sent as HumanMessage to keep it out of trusted system instructions.
    """
    llm = make_chat_model()
    structured = llm.with_structured_output(TriageVerdict, **_structured_output_kwargs())
    system = yaml.safe_load((_PROMPTS / 'triage_prompt.yaml').read_text())['template']
    messages = [SystemMessage(content=system), HumanMessage(content=state['prompt'])]
    result = _invoke_structured(structured, messages)
    verdict = result.model_dump()
    logger.info('triage verdict: is_app_request=%s, is_sufficient=%s, reason=%r',
                verdict.get('is_app_request'), verdict.get('is_sufficient'), verdict.get('reason'))
    return {'triage': verdict}


def cohort_agent(state: GraphState) -> dict:
    """LangGraph node: emit initialCohorts; return {'cohort': ...}.

    Returns {'cohort': {'_failed': True}} on failure so the judge fan-in can still fire.
    """
    llm = make_chat_model()
    Model = build_cohort_output_model(state['rdv_dict'])
    structured = llm.with_structured_output(Model, **_structured_output_kwargs())
    try:
        result = _invoke_structured(structured, _build_cohort_prompt(state))
    except Exception as e:
        logger.error('cohort agent failed after all retries: %s', e)
        return {'cohort': {'_failed': True}}
    data = _derive_query_types(result.model_dump(), state['rdv_dict'])
    cohorts = data.get('initialCohorts') or []
    logger.info('cohort agent: %d cohort(s) [%s]', len(cohorts),
                ', '.join(str(c.get('label')) for c in cohorts))
    return {'cohort': data}


def _build_analysis_prompt(state: GraphState) -> str:
    """Assemble the analysis agent's system prompt: rules + catalog + any judge feedback."""
    template = yaml.safe_load((_PROMPTS / 'analysis_prompt.yaml').read_text())['template']
    return template.format(
        available_data=_format_catalog(state['catalog']),
        available_sources=_format_sources(state['rdv_dict']),
        user_request=state['prompt'],
        feedback=_format_feedback(state, 'analysis')
    )


def analysis_agent(state: GraphState) -> dict:
    """LangGraph node: emit title/description + analysis tabs; return {'analysis': ...}.

    Same failure contract as cohort_agent.
    """
    llm = make_chat_model()
    Model = build_analysis_output_model(state['catalog'], state['rdv_dict'])
    structured = llm.with_structured_output(Model, **_structured_output_kwargs())
    try:
        result = _invoke_structured(structured, _build_analysis_prompt(state))
    except Exception as e:
        logger.error('analysis agent failed after all retries: %s', e)
        return {'analysis': {'_failed': True}}
    data = result.model_dump()
    tabs = data.get('analysis') or []
    logger.info('analysis agent: title=%r, %d tab(s) [%s]', data.get('title'), len(tabs),
                ', '.join(str(t.get('tab')) for t in tabs))
    return {'analysis': data}
