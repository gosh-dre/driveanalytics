#' Analysis of the periodicity of event recurrence
#'
#' Main function that calls other functions. Takes the input database and
#' calculates the frequency of event, summary stats and a plot (all done by other functions)
#' these are stored as a list.
#'
#' @return list; list of summary dataframe, dataframe with frequncy of event, and a plot.
#'
#' @family gen_recurrence_analysis
#' @export
gen_recurrence_analysis <- function(df_rdv, colm, cohort_col = NULL) {
  if (is.null(df_rdv) || nrow(df_rdv) == 0) {
    return(NULL)
  }

  if (is.null(cohort_col)) {
    cohort_col = "cohort"
    df_rdv[[cohort_col]] <- "all"
  }

  logger::log_info("Started gen_recurrence_analysis")
  start.time <- Sys.time()

  ordered_df <- df_rdv %>%
    dplyr::arrange(project_id, !!as.name(colm), start_datetime) %>%
    dplyr::mutate(
      time_between = dplyr::case_when(
        dplyr::lag(project_id) == project_id &
          dplyr::lag(!!as.name(colm)) == !!as.name(colm) ~
          start_datetime - dplyr::lag(start_datetime)
      ),
      time_between = round(as.numeric(time_between, units = "days"), digits = 2)
    )
  attr(ordered_df, "colm") <- colm
  attr(ordered_df, "class") <- c("gen_recurrence_analysis", class(ordered_df))

  log_record_time_taken("gen_recurrence_analysis", start.time)

  ordered_df
}

#' Provides summary stats (mean, median, range and total events) in a dataframe for a condition of interest.
#'
#' @param df tibble; Dataframe to analyse.
#' @param variables Vector; Vector of arguments made of group_column (the column to group the dataframe by) and col_to_summarise (the column to get summary stats on)
#'
#' @return results_df tibble; Summary dataframe.
#'
#' @family gen_recurrence_analysis
#' @export
tabulate.gen_recurrence_analysis <- function(df){

  logger::log_debug("Started tabulate.gen_recurrence_analysis")
  start.time <- Sys.time()

  colm <- attr(df, "colm")
  result_df <- df %>%
    picture.platform::add_codename_col(colm, "names") %>%
    tidyr::drop_na(c(time_between)) %>%
    dplyr::group_by(cohort, names) %>%
    dplyr::summarise(
      mean_days = round(mean(time_between), digits = 2),
      median_days = round(median(time_between), digits = 2),
      range = max(time_between) - min(time_between),
      total_events = length(time_between)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = cohort,
                       values_from = c(mean_days,
                                       median_days,
                                       range,
                                       total_events))

  log_record_time_taken("tabulate.gen_recurrence_analysis", start.time, "debug")

  result_df
}

#' A boxplot plot showing the frequency of an even, e.g. lab test
#'
#' @param df; Dataframe to analyse.
#'
#' @param event_of_interest; list; list of events to plot.
#'
#' @return plt; plot; boxplot of recurrent time
#'
#' @family gen_recurrence_analysis
#' @export
plot.gen_recurrence_analysis <- function(df, event_of_interest, plot_options=TRUE){

  logger::log_debug("Started plot.gen_recurrence_analysis")
  start.time <- Sys.time()

  colm <- attr(df, "colm")
  filtered_df <- df %>%
    tidyr::drop_na(c(time_between)) %>%
    dplyr::filter(!!as.name(colm) %in% !!event_of_interest)
  plt <- ggplot2::ggplot(data = filtered_df, ggplot2::aes(x = !!as.name(colm), y = time_between, fill = cohort))
  #print(plot_options)
  if (plot_options == TRUE) {
    plt <- plt +
      ggplot2::geom_boxplot(varwidth = TRUE, outlier.colour = "purple")
  } else {
    plt <- plt +
      ggplot2::stat_summary(fun.data = get_boxplot_stats,
                            geom = "boxplot")
  }
  plt <- plt +
    ggplot2::labs(y = "Time between (days)") +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~cohort)

  log_record_time_taken("plot.gen_recurrence_analysis", start.time, "debug")

  plt
}
