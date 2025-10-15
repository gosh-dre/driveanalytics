#' Visualise the length of a selected event
#'
#' Calculates a descriptive time duration analysis (overall or stratified) for
#' any given RDV and event
#'
#' @param df_rdv tibble; contains the RDV data table with event and dates;
#' required.
#' @param event_col character; the column that indicates the event of
#' interest; required.
#' @param stratify_by list of character labels; the column to group the
#' analysis by; optional.
#' @param plot_type character; type of plot.
#' @param plot_options boolean; whether to show outlier values/log transform.
#'
#' @return Named list containing descriptive summaries data frame and plot.
#'
#' @family gen_event_time_analysis
#' @export
gen_event_time_analysis <- function(df_rdv, event_col, stratify_by = NULL,
                                    plot_type = "Boxplot", plot_options = FALSE) {

  if (is.null(df_rdv) || nrow(df_rdv) == 0 || is.null(event_col) || !(event_col %in% names(df_rdv))) {
    return(list("df" = NULL, "plt" = NULL))
  }

  # If the user hasn't requested cohort stratification, just create one
  if (is.null(stratify_by)) {
    stratify_by = "Cohort"
    df_rdv <- df_rdv %>% dplyr::mutate(Cohort = "All")
  }

  start.time <- log_start_function_return_time("gen_event_time_analysis")

  df_rdv <- df_rdv %>%
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(event_col), factor, .names = "event"),
      dplyr::across(tidyselect::all_of(stratify_by), factor, .names = "stratify"),
      event_length_days = as.numeric(as.Date(end_datetime, format = "%Y-%m-%d") -
                                       as.Date(start_datetime, format = "%Y-%m-%d"))
    )

  df_summary <- df_rdv %>%
    tidyr::drop_na(event)
  df_summary <- aggregate(event_length_days ~ event + stratify, data = df_summary, FUN=summary)
  df_summary <- as.data.frame(cbind(df_summary[-ncol(df_summary)], df_summary[[ncol(df_summary)]])) %>%
    dplyr::arrange(event)

  if (plot_type == "Boxplot") {
    vis_event <- ggplot2::ggplot(df_rdv, ggplot2::aes(x = event,
                                                      y = event_length_days)) +
      ggplot2::xlab("") +
      ggplot2::ylab("Length of event (days)") +
      ggplot2::coord_flip()
    if (plot_options) {
      vis_event <- vis_event +
        ggplot2::geom_boxplot(outlier.colour = "purple") +
        ggplot2::facet_wrap( ~ stratify)
    } else {
      vis_event <- vis_event +
        ggplot2::stat_summary(fun.data = get_boxplot_stats,
                              geom = "boxplot") +
        ggplot2::facet_wrap( ~ stratify)
    }
  } else if (plot_type == "Histogram") {
    vis_event <- ggplot2::ggplot(df_rdv,
                                 ggplot2::aes(x = event_length_days)) +
      ggplot2::geom_histogram() +
      ggplot2::xlab("Length of event (days)") +
      ggplot2::ylab("") +
      ggplot2::facet_grid(ggplot2::vars(event), ggplot2::vars(stratify)) +
      ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))
    if (plot_options) {
      vis_event <- vis_event +
        ggplot2::scale_x_continuous(trans = "log") +
        ggplot2::xlab("Log of length of event (days)") #overwrite
    }
  } else {
    vis_event <- NULL
    #stop(paste0('Plot type ', plot_type, ' is not implemented yet'))
  }

  log_record_time_taken("gen_event_time_analysis", start.time)

  list("df" = df_summary, "plt" = vis_event)

}
