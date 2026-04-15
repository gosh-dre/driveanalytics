#' Competing Risk Analysis Using Aalen-Johansen Estimator
#'
#' Analyzes disease progression from baseline stage(s) to outcome stage(s) using
#' the Aalen-Johansen cumulative incidence function with death as competing risk.
#'
#' @param df_dia Diagnosis data frame with diag_nat_code, start_datetime, project_id
#' @param df_pde Patient demographic data with project_id, deceased_flag
#' @param baseline_stages Numeric vector of baseline disease stages (e.g., c(3, 4) for CKD stages 3-4)
#' @param outcome_stages Numeric vector of outcome stages (e.g., 5 for CKD stage 5)
#' @param stratify_by Column name for cohort stratification (defaults to NULL for overall only)
#'
#' @return List with cif_plot_json (plotly figures as JSON) and cif_results (fitted AJ models)
#'
#' @family gen_competing_risk_analysis
#' @export
gen_competing_risk_analysis <- function(df_dia = NULL,
                                        df_pde = NULL,
                                        baseline_stages = c(3, 4),
                                        outcome_stages = c(5),
                                        stratify_by = NULL) {

  if (is.null(df_dia) || is.null(df_pde)) return(NULL)
  
  # Load Python module
  reticulate::source_python(system.file("python/gen_competing_risk_analysis.py", package = "driveanalytics"))

  results <- run_competing_risk_analysis(df_dia, df_pde, baseline_stages, outcome_stages, stratify_by)
  
  list(cif_plot_json = results$cif_plot_json, cif_results = results$cif_results)
}