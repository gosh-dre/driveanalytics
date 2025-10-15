#' Frequency analysis for a selected category in any generic RDV
#'
#' Calculates a generic frequency analysis for any given RDV and column to group
#' the observations by.
#'
#' @param df_rdv tibble; Data set to compute frequencies across.
#' @param df_pde tibble; Demographics RDV to compute cohort sizes (in case of
#' no all patients having an event)
#' @param event_col character; Column of df_rdv to calculate frequencies from.
#' @param cohort_col character; Column label of df_rdv to define cohorts for
#' stratification; optional.
#'
#' @return Tibble containing computed counts and frequencies, by cohort if
#' provided.
#'
#' @examplesIf interactive()
#' gen_frequency_analysis(df_dia, df_pde, "diag_name")
#' gen_frequency_analysis(df_dia, df_pde, "diag_name", "sex_name")
#'
#' @family gen_frequency_analysis
#' @export
gen_frequency_analysis <- function(df_rdv, df_pde, event_col = NULL, cohort_col = NULL, new_col = FALSE, new_col_breaks = NULL, new_col_labels = NULL) {

  # Define data masking variables to avoid warnings from devtools::check()
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  event <- count <- cohort <- frequency <- . <- NULL

  # # Default params
  # event_col <- params$event_col[[1]]
  # if (is.null(order_by))
  #   order_by <- sort(unique(params$df_rdv[[params$stratify_by]]))[[1]]

  # Invalid input
  if (is.null(df_rdv) || nrow(df_rdv) == 0 || (is.null(event_col) & !new_col)) {
    return(NULL)
  }

  # If the user hasn't requested cohort stratification, just create one
  if (is.null(cohort_col)) {
    cohort_col = "Cohort"
    df_rdv <- df_rdv %>% dplyr::mutate(Cohort = "All")
  }

  logger::log_info("Started gen_frequency_analysis")
  start.time <- Sys.time()

  if (new_col) {
    df_rdv_renamed <- df_rdv %>%
      dplyr::mutate(event_length = as.numeric(difftime(end_datetime, start_datetime, units = "days")))
    df_rdv_renamed$event <- as.factor(cut(df_rdv_renamed$event_length, breaks = new_col_breaks, labels = new_col_labels, right = FALSE))

  } else {
    if (!is.null(event_col)) {
      event_col <- event_col[[1]]
      df_rdv_renamed <- df_rdv %>%
        # Rename the event and cohort columns
        dplyr::mutate(
          dplyr::across(tidyselect::all_of(c(event_col)), factor, .names = "event"),
          dplyr::across(tidyselect::all_of(c(cohort_col)), factor, .names = "cohort")
        )
    }
  }

  df_rdv_renamed <- df_rdv_renamed %>%
    # Drop NAs
    tidyr::drop_na(event) %>%
    # Drop repeated diagnoses per patient per cohort
    dplyr::select(cohort_id, event, cohort) %>%
    dplyr::distinct()

  # No of unique patients in each cohort
  df_cohort_no_patients <- df_pde %>%
    # Rename the event and cohort columns
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(c(cohort_col)), factor, .names = "cohort")
    ) %>%
    dplyr::group_by(cohort) %>%
    dplyr::count(name = "unique_proj_ids") %>%
    dplyr::ungroup()

  # Frequency table
  df_frequency <- df_rdv_renamed %>%
    # For each value of event and stratified cohort
    dplyr::group_by(cohort, event) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop_last") %>%
    dplyr::left_join(df_cohort_no_patients, by = "cohort") %>%
    dplyr::mutate(frequency = count / unique_proj_ids) %>%
    dplyr::select(-unique_proj_ids) %>%
    dplyr::ungroup()

  # Widen table for display/plot/use
  df_frequency_wide <- df_frequency %>%
    tidyr::pivot_wider(
      names_from = cohort,
      values_from = c(count, frequency),
      values_fill = 0,
      names_glue = "{cohort}.{.value}",
      names_vary = "slowest"
    )

  # Add the totals as an attribute
  df_tot <- df_frequency_wide %>%
    dplyr::summarise(event = "TOTAL", dplyr::across(where(is.numeric), sum))
  attr(df_frequency_wide, "totals") <- df_tot

  # Add the original cohorts as an attribute
  if (is.factor(df_rdv$cohort)) {
    attr(df_frequency_wide, "cohorts") <- levels(df_rdv$cohort)
  } else {
    attr(df_frequency_wide, "cohorts") <- sort(unique(df_rdv$cohort))
  }

  # Set S3 class and return
  attr(df_frequency_wide, "class") <- c("gen_frequency_analysis", class(df_frequency_wide))
  log_record_time_taken("gen_frequency_analysis", start.time)
  df_frequency_wide
}


#' Take the head (most common) values from a frequency analysis result table.
#'
#' Orders data by frequency results of the input cohort and takes the top n rows
#'
#' @param df gen_frequency_analysis; Frequency analysis result tibble.
#' @param n integer; Number of top rows to retain.
#' @param by character; Cohort to order by; optional.
#'
#' @return Tibble containing to ordered top n rows the input df
#'
#' @examplesIf interactive()
#' head(gen_frequency_analysis(df_dia, df_pde, "diag_name"), 20)
#'
#' @family gen_frequency_analysis
#' @export
head.gen_frequency_analysis <- function(df, n, by = NULL) {
  # logger::log_debug("Started head.gen_frequency_analysis")
  # start.time <- Sys.time()

  head_df_cohort(df, n, "count", by)
  # log_record_time_taken("head.gen_frequency_analysis", start.time, "debug")
}


#' Plot a bar plot of the values from a frequency analysis result table.
#'
#' Creates a ggplot2 bar plot, by cohort, of the frequency analysis results.
#'
#' @param df gen_frequency_analysis; Frequency analysis result tibble.
#' @param value character; Whether to plot "frequency" or "count".
#'
#' @return ggplot of results
#'
#' @examplesIf interactive()
#' plot(gen_frequency_analysis(df_dia, df_pde, "diag_name"))
#'
#' @family gen_frequency_analysis
#' @export
plot.gen_frequency_analysis <- function(df, value = "frequency") {
  logger::log_debug("Started plot.gen_frequency_analysis")
  start.time <- Sys.time()

  # Default unit and plot scale factors for percentages
  unit <- ""
  scale_factor <- 1
  if (value == "frequency") {
    unit <- "(%)"
    scale_factor <- 100
  }

  # Label formatting
  # cropfac <- function(x) factor(stringr::str_trunc(x,60))
  cropfac <- function(x) {
    y <- stringr::str_wrap(x, 40)
    factor(y, levels = y)
  }
  df <- dplyr::mutate(df, event = cropfac(event))

  # String pre-formatting
  col_ends <- paste0(".", value)
  col_regex <- paste0("(.*)", col_ends)
  ylabel <- paste(stringr::str_to_title(value), "in Cohort", unit)

  log_record_time_taken("plot.gen_frequency_analysis", start.time, "debug")

  # ggplot
  df %>%
    dplyr::select(event, dplyr::ends_with(col_ends)) %>%
    tidyr::pivot_longer(!event, names_pattern = col_regex) %>%
    ggplot2::ggplot(ggplot2::aes(x = event, y = scale_factor * value, fill = factor(name, levels = unique(name)))) +
    ggplot2::geom_bar(
      stat="identity",
      position = ggplot2::position_dodge2(preserve="single")
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 70, vjust = 1, hjust = 1),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::ylab(ylabel) +
    ggplot2::labs(fill = 'Cohort')
}


#' Format a table of the values from a frequency analysis result table.
#'
#' Creates a table of your choice, by cohort, of the frequency analysis results.
#'
#' @param df gen_frequency_analysis; Frequency analysis result tibble.
#' @param format character; Required format for the table ("", "latex", "DT").
#'
#' @return table of results
#'
#' @examplesIf interactive()
#' tabulate(gen_frequency_analysis(df_dia, df_pde, "diag_name"))
#'
#' @family gen_frequency_analysis
#' @export
tabulate.gen_frequency_analysis <- function(df, format = NULL) {
  logger::log_debug("Started tabulate.gen_frequency_analysis")
  start.time <- Sys.time()

  # # Add a total row (attribute) at the top of the table
  # df_tot <- dplyr::bind_rows(attr(df, "totals"), df)
  #
  # # Rename columns
  # df_ren <- df_tot %>%
  #   dplyr::rename(" " = event)

  df_ren <- dplyr::rename(df, " " = event)

  log_record_time_taken("tabulate.gen_frequency_analysis", start.time, "debug")

  # Generate standardised table
  cols_percent <- which(stringr::str_ends(names(df_ren), ".frequency"))
  tabulate_df_cohort(df_ren, format, cols_percent = cols_percent)
}
