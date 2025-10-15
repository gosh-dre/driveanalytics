#' Analysis of variability for value-based data
#'
#' Description
#'
#'
#' @return DESCRIPTION.
#'
#' @family gen_value_variability_analysis
#' @export
gen_value_variability_analysis <- function(
  df_rdv, value_col, datetime_col, group_col = "cohort_id",
  time_period = c("day", "week", "month", "year"), cohort_col = "cohort"
) {

  start.time <- log_start_function_return_time("gen_value_variability_analysis")

  df_params <- df_rdv %>%
    dplyr::transmute(
      group = !!as.name(group_col),
      value = !!as.name(value_col),
      cohort = !!as.name(cohort_col),
      period = lubridate::floor_date(!!as.name(datetime_col), unit = time_period[[1]])
    ) %>%
    dplyr::group_by(group, cohort, period) %>%
    dplyr::summarise(
      N = dplyr::n(),
      IQR = IQR(value, na.rm = TRUE),
      SD = sd(value, na.rm = TRUE),
      SD = dplyr::if_else(is.na(SD), 0, SD),
      RANGE = diff(range(value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      project_id = group
    )


  gdp_n <- driveanalytics::gen_distribution_plots(df_params, "N", "cohort", side_by_side = F, legend = F, xlab = "Number of values")
  gdp_df <- driveanalytics::gen_distribution_plots(df_params, "SD", "cohort", side_by_side = F, legend = F, xlab = "Standard deviation")
  gdp_iqr <- driveanalytics::gen_distribution_plots(df_params, "IQR", "cohort", side_by_side = F, legend = F, xlab = "Interquartile range")
  gdp_range <- driveanalytics::gen_distribution_plots(df_params, "RANGE", "cohort", side_by_side = F, legend = F, xlab = "Range")

  gdp_all <- gridExtra::grid.arrange(
    gdp_n$plot, gdp_iqr$plot, gdp_df$plot, gdp_range$plot,
    newpage = FALSE
  )

  log_record_time_taken("gen_value_variability_analysis", start.time)

  gdp_all
}
