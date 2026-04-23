import numpy as np
import pandas as pd
from lifelines import AalenJohansenFitter
import plotly.graph_objects as go
import plotly.colors


def identify_disease_stages(df, baseline_stages, outcome_stages, disease_code="N18"):
    """Identify baseline and outcome disease stages using ICD-10 code patterns."""
    baseline_stages = [str(int(s)) for s in (baseline_stages if isinstance(baseline_stages, (list, tuple)) else [baseline_stages])]
    outcome_stages = [str(int(s)) for s in (outcome_stages if isinstance(outcome_stages, (list, tuple)) else [outcome_stages])]
    
    baseline_pattern = f"{disease_code}[{''.join(baseline_stages)}]"
    outcome_pattern = f"{disease_code}[{''.join(outcome_stages)}]"
    
    baseline_mask = df["diag_nat_code"].str.contains(baseline_pattern, regex=True, na=False)
    outcome_mask = df["diag_nat_code"].str.contains(outcome_pattern, regex=True, na=False)
    
    df.loc[baseline_mask, "baseline_stage"] = df.loc[baseline_mask, "diag_nat_code"].str[-1].astype(int)
    df.loc[outcome_mask, "outcome_stage"] = df.loc[outcome_mask, "diag_nat_code"].str[-1].astype(int)
    
    return df


def process_data(df_dia, df_pde, baseline_stages, outcome_stages, disease_code="N18"):
    """Merge diagnosis and demographic data, filter by cohort entry date if present."""
    dia_cols = ["project_id", "diag_nat_code", "start_datetime"]
    if "cohort" in df_dia.columns:
        dia_cols.extend(["cohort", "cohort_entry_date"])
    dia_subset = df_dia[[c for c in dia_cols if c in df_dia.columns]].copy()

    pde_subset = df_pde[["project_id", "deceased_flag"]].drop(columns=["cohort"], errors="ignore").copy()

    merged = dia_subset.merge(pde_subset, on="project_id", how="left")
    
    # Filter to only diagnoses after cohort entry
    if "cohort_entry_date" in merged.columns:
        merged['start_datetime'] = pd.to_datetime(merged['start_datetime'])
        merged['cohort_entry_date'] = pd.to_datetime(merged['cohort_entry_date'])
        merged = merged[merged['start_datetime'] >= merged['cohort_entry_date']].copy()

    staged = identify_disease_stages(merged, baseline_stages, outcome_stages, disease_code)
    return staged[staged["baseline_stage"].notna() | staged["outcome_stage"].notna()].copy()


def build_progression_table(relevant_records):
    """
    Build patient-level progression table for Aalen-Johansen fitting.
    
    Returns one row per patient with:
    - duration: days from baseline to outcome (or last observation)
    - event_type: 1=outcome reached, 2=death w/o outcome, 0=censored
    """
    has_baseline = relevant_records["baseline_stage"].notna()
    has_outcome  = relevant_records["outcome_stage"].notna()

    # Get first baseline diagnosis per patient
    baseline_dates = (
        relevant_records[has_baseline]
        .sort_values("start_datetime")
        .drop_duplicates("project_id", keep="first")
        [["project_id", "start_datetime", "deceased_flag"]]
        .rename(columns={"start_datetime": "baseline_date"})
    )

    # Get first outcome diagnosis per patient
    outcome_dates = (
        relevant_records[has_outcome]
        .sort_values("start_datetime")
        .drop_duplicates("project_id", keep="first")
        [["project_id", "start_datetime"]]
        .rename(columns={"start_datetime": "outcome_date"})
    )

    # Get last observation per patient
    last_dates = (
        relevant_records.groupby("project_id")["start_datetime"]
        .max().reset_index()
        .rename(columns={"start_datetime": "last_date"})
    )

    df = (
        baseline_dates
        .merge(outcome_dates, on="project_id", how="left")
        .merge(last_dates,    on="project_id", how="left")
    )

    df["baseline_date"] = pd.to_datetime(df["baseline_date"])
    df["outcome_date"]  = pd.to_datetime(df["outcome_date"])
    df["last_date"]     = pd.to_datetime(df["last_date"])

    # Calculate duration from baseline to outcome or last observation
    df["days_to_outcome"]  = (df["outcome_date"] - df["baseline_date"]).dt.days
    df["days_to_last"]     = (df["last_date"]    - df["baseline_date"]).dt.days
    df["duration"]         = df["days_to_outcome"].combine_first(df["days_to_last"]).clip(lower=0)

    # Classify event type (competing risks framework)
    df["event_type"] = 0  # Censored
    df.loc[df["days_to_outcome"].notna(), "event_type"] = 1  # Outcome reached
    df.loc[(df["deceased_flag"] == "Y") & df["days_to_outcome"].isna(), "event_type"] = 2  # Died w/o outcome

    return df[df["duration"].notna()].copy()


def perform_survival_analysis(relevant_records, stratify_by=None):
    """Fit Aalen-Johansen models: overall + stratified by cohort if specified."""
    results = {}

    # Overall model (all patients)
    overall_data = build_progression_table(relevant_records)
    if len(overall_data) > 0:
        ajf = AalenJohansenFitter()
        ajf.fit(durations=overall_data["duration"], event_observed=overall_data["event_type"], event_of_interest=1)
        results["overall"] = ajf

    # Stratified models (one per cohort)
    if stratify_by and stratify_by in relevant_records.columns:
        for group, group_records in relevant_records.groupby(stratify_by, observed=False):
            if len(group_records) == 0:
                continue
            try:
                group_data = build_progression_table(group_records)
                if len(group_data) == 0:
                    continue
                ajf = AalenJohansenFitter()
                ajf.fit(durations=group_data["duration"], event_observed=group_data["event_type"], event_of_interest=1)
                results[str(group)] = ajf
            except Exception as e:
                print(f"Warning: Could not fit model for '{group}': {e}")

    return results


def run_competing_risk_analysis(df_dia, df_pde, baseline_stages, outcome_stages, stratify_by=None):
    """
    Main entry point: process data, fit Aalen-Johansen models, generate plotly JSON.
    
    Returns dict with cif_results (fitted models) and cif_plot_json (plotly figures).
    """
    relevant = process_data(df_dia, df_pde, baseline_stages, outcome_stages)
    ajf_models = perform_survival_analysis(relevant, stratify_by=stratify_by)

    n = len(ajf_models)
    colors = plotly.colors.sample_colorscale("bluered", max(n, 2))[:n]

    plot_json = {}
    for idx, (name, ajf) in enumerate(ajf_models.items()):
        cif = ajf.cumulative_density_
        ci_lower = ajf.confidence_interval_.iloc[:, 0]
        ci_upper = ajf.confidence_interval_.iloc[:, 1]
        color = colors[idx]
        fill_color = color.replace("rgb(", "rgba(").replace(")", ", 0.2)")
        title = "Competing Risk Analysis: Progression to Stage 5 CKD" if name == "overall" else f"Competing Risk Analysis: {name}"

        # Convert to lists for JSON serialization (NaN → point estimate)
        # Plotly JSON unable to handle numpy arrays, so convert to lists and handle NaN for CI bounds
        x = cif.index.tolist()
        y = cif.iloc[:, 0].tolist()
        y_raw = cif.iloc[:, 0].values
        ci_lo = [v if not np.isnan(v) else y_raw[i] for i, v in enumerate(ci_lower.tolist())]
        ci_hi = [v if not np.isnan(v) else y_raw[i] for i, v in enumerate(ci_upper.tolist())]

        # Create plotly figure with CI band
        fig = go.Figure([
            go.Scatter(x=x, y=ci_lo, mode="lines", line=dict(width=0, shape="hv"), showlegend=False, hoverinfo="skip"),
            go.Scatter(x=x, y=ci_hi, mode="lines", line=dict(width=0, shape="hv"), fill="tonexty", fillcolor=fill_color, showlegend=False, hoverinfo="skip"),
            go.Scatter(x=x, y=y, mode="lines", line=dict(color=color, width=2, shape="hv"), name="CIF", hovertemplate="Day: %{x}<br>CIF: %{y:.3f}<extra></extra>")
        ])

        fig.update_layout(
            title=dict(text=title, font=dict(size=14)),
            xaxis=dict(title="Days from Baseline"),
            yaxis=dict(title="Cumulative Incidence", range=[0, 1]),
            showlegend=False, plot_bgcolor="white", paper_bgcolor="white",
            hovermode="x unified"
        )

        plot_json[name] = fig.to_json()

    return {"cif_results": ajf_models, "cif_plot_json": plot_json}
