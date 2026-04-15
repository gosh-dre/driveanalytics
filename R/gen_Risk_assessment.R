#' CKD to CVD Risk Assessment Module (Python Bridge)
#'
#' @param df_ckd tibble; contains clinical metrics; required.
#' @param threshold_value numeric; optional threshold adjustment.
#' @return Named list containing summary statistics, plot paths (PNGs), and processed data.
#' @export
gen_risk_assessment <- function(df_ckd = NULL, threshold_value = NULL) {

  # 1. Input Validation
  if (is.null(df_ckd) || nrow(df_ckd) == 0) {
    return(list(df = NULL, plots = NULL))
  }

  required_cols <- c("Troponin", "eGFR", "BNP")
  if (!all(required_cols %in% names(df_ckd))) {
    stop(paste("Missing required columns:", paste(required_cols, collapse = ", ")))
  }

  logger::log_info("Started gen_risk_assessment (via Reticulate)")

  # 2. Python Execution
  py_path <- system.file("python/risk_assessment.py", package = "driveanalytics")
  
  if (!file.exists(py_path)) {
    logger::log_error(paste("Python script not found at:", py_path))
    stop("risk_assessment.py not found. Check project root.")
  }

  reticulate::source_python(py_path)
  
  # 3. Call Python Function
  py_results <- tryCatch({
    assess_ckd_risk(df_ckd, threshold_value)
  }, error = function(e) {
    logger::log_error(paste("Python Execution Error:", e$message))
    return(NULL)
  })

  if (is.null(py_results)) return(list(df = NULL, plots = NULL))

  # 4. Return Python results directly - let Shiny handle conversion
  results <- list(
    df = py_results$summary_stats,
    plot_json = py_results$plots,
    raw_data = py_results$df
  )

  logger::log_info("Finished gen_risk_assessment")

  return(results)
}