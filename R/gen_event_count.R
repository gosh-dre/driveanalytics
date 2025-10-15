#' Event count analysis
#'
#' Description
#'
#'
#' @return DESCRIPTION.
#'
#' @family gen_event_count
#' @export
gen_event_count <- function(df_rdv, df_pde, event_col, cohort_col = "cohort", count_unique = TRUE) {

  # Invalid input
  if (is.null(df_rdv) || nrow(df_rdv) == 0) {
    return(NULL)
  }

  start.time <- log_start_function_return_time("gen_event_count", "debug")

  df_rdv_renamed <- df_rdv %>%
    # Rename the event and cohort columns
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(c(event_col[[1]])), factor, .names = "event"),
      dplyr::across(tidyselect::all_of(c(cohort_col)), factor, .names = "cohort")
    ) %>%
    # Drop NAs
    tidyr::drop_na(event) %>%
    # Drop repeated diagnoses per patient per cohort
    dplyr::select(cohort_id, event, cohort)

  # Remove duplicates, if required
  if (count_unique) {
    df_rdv_renamed <- df_rdv_renamed %>%
      dplyr::distinct(dplyr::across(dplyr::everything()))
  }

  # No of unique patients in each cohort
  df_cohort_no_patients <- df_pde %>%
    # Rename the event and cohort columns
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(c(cohort_col)), factor, .names = "cohort")
    ) %>%
    dplyr::group_by(cohort) %>%
    dplyr::count(name = "n_proj_ids") %>%
    dplyr::ungroup()

  # Compute counts
  df_nonzero_counts <- df_rdv_renamed %>%
    dplyr::group_by(cohort_id, cohort) %>%
    dplyr::count(name = "event_count") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(cohort, event_count) %>%
    dplyr::count(name = "patient_count") %>%
    dplyr::ungroup()

  # Compute the number of patients with 0 events
  df_zero_counts <- df_nonzero_counts %>%
    dplyr::group_by(cohort) %>%
    dplyr::summarise(n_nonzero_ids = sum(patient_count)) %>%
    dplyr::right_join(df_cohort_no_patients, by = "cohort") %>%
    dplyr::mutate(
      event_count = 0,
      patient_count = n_proj_ids - n_nonzero_ids
    ) %>%
    dplyr::select(tidyselect::all_of(names(df_nonzero_counts)))

  # Finalise the counts by merging zero and non-zeros
  df_counts <- df_nonzero_counts %>%
    dplyr::bind_rows(df_zero_counts) %>%
    dplyr::arrange(cohort, event_count) %>%
    dplyr::group_by(cohort) %>%
    dplyr::mutate(patient_pct = patient_count / sum(patient_count)) %>%
    dplyr::ungroup()

  log_record_time_taken("gen_event_count", start.time)

  # Set S3 class and return
  attr(df_counts, "class") <- c("gen_event_count", class(df_counts))
  return(df_counts)
}


#' Plot ?.
#'
#' Creates a ggplot2 ?
#'
#' @param ?
#'
#' @return ggplot of results
#'
#' @family gen_event_count
#' @export
plot.gen_event_count <- function(df) {

  start.time <- log_start_function_return_time("plot.gen_event_count", "debug")

  df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = event_count,
        weight = patient_count,
        color = cohort,
        fill = cohort
      )
    ) +
    ggplot2::geom_histogram(
      alpha=0.5, binwidth = 1, position="identity"
    ) +
    ggplot2::labs(
      x = "Number of events",
      y = "Count"
    )

  # df %>%
  #   ggplot2::ggplot(
  #     ggplot2::aes(
  #       x = event_count,
  #       y = patient_count,
  #       color = cohort,
  #       fill = cohort
  #     )
  #   ) +
  #   ggplot2::geom_bar(
  #     stat="identity",
  #     # bins = max(df$event_count)+1,
  #     position = ggplot2::position_dodge2(preserve="single")
  #     ) +
  #   ggplot2::labs(
  #     x = "Number of events",
  #     y = "Count"
  #   ) +
  #   ggplot2::scale_x_continuous(breaks = seq(0, max(df$event_count)))

}


#' Tabulate ?.
#'
#' Creates a table ?
#'
#' @param ?
#'
#' @return table of results
#'
#' @family gen_event_count
#' @export
tabulate.gen_event_count <- function(df, format = NULL) {

  start.time <- log_start_function_return_time("tabulate.gen_event_count", "debug")

  df_wide <- df %>%
    dplyr::rename(count = patient_count, frequency = patient_pct) %>%
    tidyr::pivot_wider(
      names_from = cohort,
      values_from = c(count, frequency),
      names_glue = "{cohort}.{.value}",
      names_vary = "slowest",
      values_fill = 0
    ) %>%
    dplyr::arrange(event_count) %>%
    dplyr::rename("Number of events" = event_count)

  cols_percent <- which(stringr::str_ends(names(df_wide), ".frequency"))

  log_record_time_taken("tabulate.gen_event_count", start.time, "debug")
  tabulate_df_cohort(df_wide, format, cols_percent)

}
