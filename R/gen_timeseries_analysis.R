#' Simple time series analysis for a selected column in any RDV
#'
#' Generates simple summary statistics and plot for any given RDV and series
#' column, optionally grouped by specified column.
#'
#' @param df_rdv tibble; contains the RDV; required.
#' @param event_col character; the name of the event column; required.
#' @param datetime_col character; the name of the event date column; required.
#' @param series_col character; the name of the time series column; required.
#' @param cohort_col character; name of column to stratify by; required.
#' @param group_col character; name of additional grouping column.
#' @param type character; name of group_by datatype column.
#' @param units character; the name of the measurement units column; optional.
#' @param ref_range character vector; names of reference range columns.
#' @param hor_axis character; horizontal axis values.
#'
#' @return Named list containing the summary statistics data frame and plot.
#'
#' @family gen_timeseries_analysis
#' @export
gen_timeseries_analysis <- function(df_rdv, df_pde, grouping_col, value_col, datetime_col,
                                    cohort_col, value_type_col = NULL,
                                    units_col = NULL, ref_range_col = NULL,
                                    hor_axis = "Years since entering cohort",
                                    overlay_cohorts = FALSE,
                                    aggregate_cohorts = "None") {

  if (is.null(df_rdv) | nrow(df_rdv) == 0) {
    return(NULL)
  }

  start.time <- log_start_function_return_time("gen_timeseries_analysis")

  datetime <- date_my <- series_median <- series_month_median <- . <- NULL

  df_rdv_summary_number <- NULL
  plt_series <- NULL
  df_rdv_summary_category <- NULL

  # Default date type is Number
  if (!is.null(value_type_col)) {
    list_df <- split(df_rdv, df_rdv[[value_type_col]])
  } else {
    list_df <- list("Number" = df_rdv)
  }

  if (!is.null(list_df[["Number"]]) & length(list_df[["Number"]]) > 0) {

    df_rdv <- list_df$Number

    if (!any(!is.na(as.numeric(df_rdv[[value_col]])))) {
      return()
    }

    df_rdv <- df_rdv %>%
      dplyr::group_by(!!as.name(grouping_col)) %>%
      dplyr::group_modify(function(x, y) dplyr::mutate(x, event = factor(paste(y, collapse = " / ")))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        # dplyr::across(tidyselect::all_of(event_col), factor, .names = "event"),
        dplyr::across(tidyselect::all_of(value_col), as.numeric, .names = "value"),
        datetime = as.POSIXct(!!as.name(datetime_col)),
        dplyr::across(tidyselect::all_of(cohort_col), factor, .names = "cohort"),
        dplyr::across(tidyselect::all_of(units_col), factor, .names = "units")
      )

    # Add the x_val column based on selection
    if (hor_axis == "Event date") {

      df_rdv_plot <- df_rdv %>%
        dplyr::mutate(x_val = lubridate::floor_date(datetime, unit = "month"))

    } else if (hor_axis == "Event number") {

      df_rdv_plot <- df_rdv %>%
        dplyr::group_by(project_id, event, cohort) %>%
        dplyr::arrange(datetime) %>%
        dplyr::mutate(x_val = rank(datetime, ties.method = "first")) %>%
        dplyr::ungroup()

    } else if (hor_axis == "Days since first event") {

      df_rdv_plot <- df_rdv %>%
        dplyr::group_by(project_id, event, cohort) %>%
        dplyr::mutate(x_val = as.numeric(difftime(datetime, min(datetime), units = "days"))) %>%
        dplyr::ungroup()

    } else if (hor_axis == "Years since entering cohort") {

      df_rdv_plot <- df_rdv %>%
        dplyr::mutate(x_val = as.numeric(difftime(datetime, cohort_entry_date, units = "days"))/365.25)

    } else if (hor_axis == "Years since birth") {

      df_rdv_plot <- df_rdv %>%
        dplyr::left_join(
          df_pde %>% dplyr::select(project_id, birth_date),
          by = "project_id"
        ) %>%
        dplyr::mutate(x_val = as.numeric(difftime(datetime, birth_date, units = "days"))/365.25) %>%
        dplyr::ungroup()

    }

    df_rdv_grouped <- df_rdv_plot %>%
      dplyr::group_by(event, cohort, x_val)

    plt_series <- .plot_timeseries(df_rdv_plot, hor_axis, overlay_cohorts, aggregate_cohorts)

    # Add reference range, if available
    if (!is.null(ref_range_col) & all(ref_range_col %in% names(df_rdv))) {
      for (ss in ref_range_col) {
        plt_series <- plt_series +
          ggplot2::geom_line(
            ggplot2::aes(y = mean(as.numeric(!!as.name(ss)), na.rm = TRUE)),
            color="maroon", linetype="dashed"
          )
      }
    }

    df_rdv_summary_number <- df_rdv_grouped %>%
      tidyr::drop_na(event) %>%
      dplyr::summarise(series_median = round(stats::median(value, na.rm = TRUE)),
                       series_mean = round(mean(value, na.rm = TRUE)),
                       series_iqr = round(stats::IQR(value, na.rm = TRUE)),
                       series_count = dplyr::n(),
                       id_count = dplyr::n_distinct(project_id)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = cohort,
                         values_from = c(series_median,
                                         series_mean,
                                         series_iqr,
                                         series_count,
                                         id_count))

  }

  if (!is.null(list_df[["Category"]]) & length(list_df[["Category"]]) > 0) {

    df_rdv <- list_df$Category

    df_rdv_summary_category <- NULL

    # df_rdv <- df_rdv %>%
    #   dplyr::mutate(
    #     dplyr::across(tidyselect::all_of(event_col), factor, .names = "event"),
    #     dplyr::across(tidyselect::all_of(datetime_col), as.POSIXct, .names = "datetime"),
    #     dplyr::across(tidyselect::all_of(series_col), factor, .names = "series"),
    #     dplyr::across(tidyselect::all_of(cohort_col), factor, .names = "cohort"),
    #     dplyr::across(tidyselect::all_of(group_col), factor, .names = "groupf"),
    #     dplyr::across(tidyselect::all_of(units), factor, .names = "units"),
    #     grouping = paste0(groupf, " (", units, ") ")
    #   )
    #
    # df_rdv_summary_category <- df_rdv %>%
    #   dplyr::group_by(event, cohort, grouping, series) %>%
    #   dplyr::summarise(patient_count = dplyr::n_distinct(project_id)) %>%
    #   dplyr::ungroup() %>%
    #   tidyr::pivot_wider(names_from = cohort,
    #                      values_from = c(patient_count))

  }

  log_record_time_taken("gen_timeseries_analysis", start.time)

  list("df" = df_rdv_summary_number,
       "plt" = plt_series,
       "df_cat" = df_rdv_summary_category)

}

#' Helper function to plot time series
#'
#' @param df dataframe; data to plot
#' @param hor_axis_label character; label for horizontal axis
#'
#' @noRd
#' @keywords internal

.plot_timeseries <- function(df, hor_axis_label, overlay_cohorts, aggregate_cohorts) {

  start.time <- log_start_function_return_time(".plot_timeseries", "debug")

  # Whether to overlay cohorts on one facet or two
  if (overlay_cohorts) facet_formula <- ggplot2::vars(event)
  else                 facet_formula <- as.formula(event ~ cohort)

  # Constants
  quant_alpha = 0.5
  pt_alpha = 0.5

  # Remove NAs
  df_selected <- df %>%
    dplyr::filter(!is.na(event), !is.na(value)) %>%
    dplyr::select(project_id, x_val, value, event, cohort)

  # Check inputs
  if (nrow(df_selected) == 0) return(NULL)

  # Identify cohorts that need aggregation, if required
  cohorts_to_aggregate <- factor()
  if (aggregate_cohorts != "None") {
    cohorts_to_aggregate <- df_selected %>%
      dplyr::group_by(cohort) %>%
      dplyr::summarise(n = length(unique(project_id))) %>%
      dplyr::filter(n > 1) %>%
      dplyr::pull(cohort)
  }

  # Points to plot (non-aggregated cohorts)
  df_pts <- df_selected %>%
    dplyr::filter(!cohort %in% cohorts_to_aggregate)

  # Quantiles to plot (aggregated cohorts)
  plt_agg <- NULL
  if (aggregate_cohorts == "Constant centiles") {

    df_agg <- df_selected %>%
      dplyr::filter(cohort %in% cohorts_to_aggregate) %>%
      dplyr::group_by(event, cohort) %>%
      dplyr::summarise(
        qprob = seq(0.1, 0.9, 0.1),
        qval = quantile(value, probs = qprob, na.rm = TRUE),
        qlbl = paste(qprob * 100, "%")
      ) %>%
      dplyr::ungroup()

    plt_agg <- list(
      ggplot2::geom_hline(data = df_agg, ggplot2::aes(yintercept = qval, color = cohort), alpha = quant_alpha),
      ggplot2::geom_text(data = df_agg, ggplot2::aes(0, qval, label = qlbl, color = cohort, vjust = 1.1, hjust = 0.5), alpha = quant_alpha)
    )

  } else if (aggregate_cohorts == "Time varying centiles") {

    df_agg <- df_selected %>%
      dplyr::filter(cohort == cohorts_to_aggregate) %>%
      dplyr::mutate(x_period = cut(x_val, breaks = 10)) %>%
      dplyr::group_by(event, cohort, x_period) %>%
      dplyr::summarise(
        qprob = seq(0.1, 0.9, 0.1),
        qval = quantile(value, probs = qprob, na.rm = TRUE),
        qlbl = paste(qprob * 100, "%")
      ) %>%
      dplyr::mutate(
        x_val = stringr::str_match(x_period, "^\\((.*),(.*)]")[,2:3] %>%
          as.numeric() %>% matrix(ncol = 2) %>% rowMeans()
      ) %>%
      dplyr::ungroup()

    df_agg_last <- df_agg %>%
      dplyr::filter(x_val == max(x_val))

    plt_agg <- list(
      ggplot2::geom_line(data = df_agg, ggplot2::aes(x_val, qval, group = interaction(qlbl, cohort), color = cohort), alpha = quant_alpha),
      ggplot2::geom_text(data = df_agg_last, ggplot2::aes(x_val, qval, label = qlbl, color = cohort, vjust = 1.1, hjust = 0.5), alpha = quant_alpha)
    )
  }

  log_record_time_taken(".plot_timeseries", start.time, "debug")

  # Plot
  ggplot2::ggplot(data = df_selected, ggplot2::aes(x_val, value, color = cohort)) +
    plt_agg +
    ggplot2::geom_point(data = df_pts, alpha = pt_alpha , na.rm = TRUE) +
    ggplot2::geom_smooth(data = df_pts, ggplot2::aes(colour = cohort), se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs"), color = "black") +
    ggplot2::xlab(hor_axis_label) +
    ggplot2::ylab("") +
    ggplot2::facet_grid(facet_formula, scales = "free_y", labeller = ggplot2::label_wrap_gen(width = 30, multi_line = TRUE)) +
    ggplot2::theme(
      # strip.text.y = ggplot2::element_text(angle = 0),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.spacing = grid::unit(3, "lines"),
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    ) +
    NULL

}
