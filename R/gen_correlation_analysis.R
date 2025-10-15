#' Assess the correlation of a set of events by cohort
#'
#' Description
#'
#' @param df_gen_freq_analysis a frequency analysis output table
#' @param relative_to which cohort to analyse relative to
#'
#' @return A gen_correlation_analysis table containing the relative risks and
#' odds ratios
#'
#' @family gen_correlation_analysis
#' @export
gen_correlation_analysis <- function(df_gen_freq_analysis, relative_to = NULL) {

  if (is.null(df_gen_freq_analysis) || nrow(df_gen_freq_analysis) == 0) return(NULL)

  # Default relative to is first cohort
  if (is.null(relative_to) || relative_to == "") {
    relative_to <- attr(df_gen_freq_analysis, "cohorts")[[1]]
  }

  start.time <- log_start_function_return_time("gen_correlation_analysis")

  # Compute probs/odds, ratios and confidence intervals
  df_output <- df_gen_freq_analysis %>%
    tidyr::pivot_longer(!event, names_pattern = "(.*)\\.(.*)", names_to = c("cohort", ".value") ) %>%
    # Infer cohort sizes
    dplyr::group_by(cohort) %>% dplyr::mutate(n = median(count/frequency, na.rm = TRUE)) %>% dplyr::ungroup() %>%
    dplyr::select(-frequency) %>%
    dplyr::group_by(event) %>%
    dplyr::mutate(
      # If any of the cohorts have zero entries - add 0.5 to all of them
      iszero = round(min(count)) == 0,
      count = ifelse(iszero, count + 0.5, count),
      n = ifelse(iszero, n + 1, n),
      # Precalc the anti-count (number without event)
      anticount = n - count,
      # Probability and odds
      prob = count / n,
      odds = count / anticount,
      # Relative values
      control_idx = cohort == relative_to,
      prob_control_idx = ifelse(sum(control_idx) > 0, prob[control_idx], NA),
      odds_control_idx = ifelse(sum(control_idx) > 0, odds[control_idx], NA),
      n_control_idx = ifelse(sum(control_idx) > 0, n[control_idx], NA),
      count_control_idx = ifelse(sum(control_idx) > 0, count[control_idx], NA),
      anticount_control_idx = ifelse(sum(control_idx) > 0, anticount[control_idx], NA),
      # Ratios
      rr_value = prob / prob_control_idx,
      or_value = odds / odds_control_idx,
      # Standard errors
      se_ln_rr = sqrt((anticount / (count * n)) + (anticount_control_idx / (count_control_idx * n_control_idx))),
      se_ln_or = sqrt(1/count + 1/anticount + 1/count_control_idx + 1/anticount_control_idx),
      # Confidence intervals
      rr_ci_lower = exp(log(rr_value) - 1.96 * se_ln_rr),
      rr_ci_upper = exp(log(rr_value) + 1.96 * se_ln_rr),
      or_ci_lower = exp(log(or_value) - 1.96 * se_ln_or),
      or_ci_upper = exp(log(or_value) + 1.96 * se_ln_or),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(event, cohort, dplyr::starts_with("or"), dplyr::starts_with("rr")) %>%
    dplyr::filter(cohort != relative_to) %>%
    tidyr::pivot_wider(
      names_from = cohort,
      values_from = c(dplyr::starts_with("rr"), dplyr::starts_with("or")),
      names_glue = "{cohort}.{.value}",
      names_vary = "slowest"
    )

  log_record_time_taken("gen_correlation_analysis", start.time)

  # Set S3 class and return
  attr(df_output, "class") <- c("gen_correlation_analysis", class(df_output))
  df_output
}


#' @export
select_metric <- function(df, type) {

  start.time <- log_start_function_return_time("select_metric", "debug")

  filter_regex <- paste0(
    "^(event)|([a-zA-Z0-9]\\.",
    stringr::str_match(type, "^((?:or)|(?:rr))_.*$")[[2]],
    "(_.*))$"
  )

  log_record_time_taken("select_metric", start.time, "debug")

  df %>%
    dplyr::select(tidyselect::matches(filter_regex))
}


#' @export
head.gen_correlation_analysis <- function(df, n, col = "rr_value", by = NULL) {

  start.time <- log_start_function_return_time("head.gen_correlation_analysis", "debug")

  if (nrow(df) == 0) return(df)

  df_filtered <- select_metric(df, col)

  log_record_time_taken("head.gen_correlation_analysis", start.time, "debug")

  head_df_cohort(df_filtered, n, col, by)
}



#' @export
plot.gen_correlation_analysis <- function(df) {

  if (nrow(df) == 0) return(NULL)

  start.time <- log_start_function_return_time("plot.gen_correlation_analysis", "debug")

  # Label formatting
  # cropfac <- function(x) factor(stringr::str_trunc(x,60))
  cropfac <- function(x) {
    y <- stringr::str_wrap(x, 30)
    factor(y, levels = y)
  }
  df <- dplyr::mutate(df, event = cropfac(event))

  # Infer plot type
  if(any(stringr::str_detect(names(df), ".rr_")))  title_str <- "Relative Risk"
  else                                              title_str <- "Odds Ratio"
  title_str <- paste(title_str, "(with 95% CI)")

  df_fmt <- df %>%
    # Lengthen the data based on cohort
    tidyr::pivot_longer(!event, names_pattern = "^(.*)\\.(.*)$", names_to = c("Cohort", ".value")) %>%
    # Remove the rr/or from variable names
    dplyr::rename_with(function(x) stringr::str_replace(x, "^((or)|(rr))_", ""))

  pos_dodge <- ggplot2::position_dodge(width = 1)

  log_record_time_taken("plot.gen_correlation_analysis", start.time, "debug")

  # Plot
  df_fmt %>%
    ggplot2::ggplot(ggplot2::aes(y = event, x = value, xmin = ci_lower,
                                 xmax = ci_upper, colour = Cohort)) +
    ggplot2::geom_errorbarh(position = pos_dodge, linewidth = 1.5) +
    ggplot2::geom_point(position = pos_dodge, size = 4) +
    ggplot2::scale_x_log10(labels = scales::format_format(scientific = FALSE)) +
    ggplot2::scale_y_discrete(limits = rev(levels(df$event))) +
    ggplot2::xlab(title_str) +
    ggplot2::theme(
      axis.title.y=ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    NULL
}



#' @export
tabulate.gen_correlation_analysis <- function(df, format = NULL) {

  start.time <- log_start_function_return_time("tabulate.gen_correlation_analysis", "debug")

  if (nrow(df) == 0) return(NULL)

  # Format column names
  names_fmt <- function(x) {
    x %>%
      stringr::str_replace("or_value$", "Odds Ratio") %>%
      stringr::str_replace("rr_value$", "Relative Risk") %>%
      stringr::str_replace("rr", "") %>%
      stringr::str_replace("or", " ") %>%
      stringr::str_replace("_ci_upper", " Upper 95% CI") %>%
      stringr::str_replace("_ci_lower", " Lower 95% CI")
  }

  # Rename columns
  df_fmt <- df %>%
    dplyr::rename_with(names_fmt, !event) %>%
    dplyr::rename("Event" = event)

  # Generate standardised table
  cols_percent <- which((names(df_fmt) != "Event"))

  log_record_time_taken("tabulate.gen_correlation_analysis", start.time, "debug")

  tabulate_df_cohort(df_fmt, format, cols_percent)

}
