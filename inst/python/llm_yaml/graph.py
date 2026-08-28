"""Build and compile the LangGraph: parallel agents -> judge -> bounded retry -> assemble."""

from __future__ import annotations

from langgraph.graph import StateGraph, START, END

from .agents import analysis_agent, cohort_agent, triage_agent
from .assemble import assemble_yaml
from .judge import judge_node, route_after_judge
from .state import GraphState


def route_after_triage(state: GraphState) -> list[str]:
    """Conditional edge: fan out to the agents when the request is accepted, else end early.

    Only is_app_request gates generation; is_sufficient is advisory (see entry.generate_yaml).
    """
    triage = state.get('triage') or {}
    if triage.get('is_app_request'):
        return ['cohort', 'analysis']
    return [END]


def build_graph():
    """Wire triage -> (cohort ∥ analysis) -> judge -(route)-> assemble; refuse at triage. Return compiled graph."""
    # Create graph
    g = StateGraph(GraphState)

    # Create nodes
    g.add_node('triage', triage_agent)
    g.add_node('cohort', cohort_agent)
    g.add_node('analysis', analysis_agent)
    g.add_node('judge', judge_node)
    g.add_node('assemble', assemble_yaml)

    # Add edges
    g.add_edge(START, 'triage')
    g.add_conditional_edges('triage', route_after_triage, ["cohort", "analysis", END])
    g.add_edge('cohort', 'judge')
    g.add_edge('analysis', 'judge')

    g.add_conditional_edges('judge', route_after_judge, ["cohort", "analysis", "assemble", END])

    g.add_edge('assemble', END)

    return g.compile()
