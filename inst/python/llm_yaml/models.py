"""Dynamic Pydantic models built at runtime so the schema enforces literals (columns, params, rpkg) rather than checking post-call."""

from __future__ import annotations

from typing import Annotated, Literal, Union

from pydantic import BaseModel, Field, create_model

INCLUSION_TYPES = ("ever", "never", "fully_concurrent", "after_first", "on_first")

FilterValue = str | list[str] | list[float]
ParamValue = str | int | float | bool | list[str]


class TriageVerdict(BaseModel):
    is_app_request: bool
    is_sufficient: bool
    reason: str


def _filter_for_rdv(slug: str, columns: list[dict]) -> type[BaseModel]:
    names = tuple(col["name"] for col in columns)
    return create_model(
        f"Filter_{slug}",
        rdv=Literal[slug],
        column=Literal[names],
        inclusion=Literal[INCLUSION_TYPES],
        val=FilterValue,
        window=(list[int] | None, None),     # default None; excluded from required
    )


def build_cohort_output_model(rdv_dict: dict) -> type[BaseModel]:
    filters = []
    for slug, info in rdv_dict.items():
        col = info.get("columns")
        if col:
            filters.append(_filter_for_rdv(slug, col))

    Filter = Annotated[Union[tuple(filters)], Field(discriminator="rdv")]
    Cohort = create_model("Cohort", label=str, config=list[Filter])
    return create_model("CohortOutput", initialCohorts=list[Cohort])


def _params_for_fn(fn: str, params: list[str], df_handles: tuple[str, ...]) -> type[BaseModel]:
    fields = {}
    for param in params:
        if param.startswith("df_") and df_handles:
            fields[param] = Literal[df_handles] | None
        else:
            fields[param] = ParamValue | None
    return create_model(f"Params_{fn}", **fields)


def _method_for_fn(fn: str, info: dict, df_handles: tuple[str, ...]) -> type[BaseModel]:
    Params = _params_for_fn(fn, info.get("params") or [], df_handles)
    return create_model(
        f"Method_{fn}",
        fn=Literal[fn],
        rpkg=Literal[info["rpkg"]],
        tab_lbl=str | None,
        params=Params
    )


def build_analysis_output_model(catalog: dict, rdv_dict: dict) -> type[BaseModel]:
    df_handles = tuple(f"df_{slug}" for slug in rdv_dict)
    methods = []
    for fn, info in catalog.items():
        methods.append(_method_for_fn(fn, info, df_handles))
    Method = Union[tuple(methods)]
    Analysis = create_model("Analysis", tab=str, methodList=list[Method])
    return create_model(
        "AnalysisOutput",
        title=str,
        description=str,
        analysis=list[Analysis]
    )
