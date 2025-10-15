#' Generate distribution plots
#'
#' Plots to examine the distribution of a column
#'
#' @param df_rdv Data frame.
#' @param col Character label of column to plot.
#' @param stratify_by Character label of grouping column.
#' @param side_by_side Logical; should the plots be side by side.
#' @param calc_length Logical; should number of days be calculated.
#' @param calc_diff Logical; should the number of days be based on difference
#' between start and end date or just distinct start dates.
#' @param filter_condition String; condition to filter RDV on.
#' @param transform String; transformation method.
#' @param density_bw ggplot2 geom_density Density bandwidth adjustment.
#' @param legend boolean; whether to show legend to plot.
#' @param xlab Character
#'
#' @return A list that contains a string of the statistical test result, plots
#' and a summary table.
#'
#' @family gen_distribution_plots
#' @export
gen_distribution_plots <- function(df_rdv, col = NULL, stratify_by = NULL,
                                   side_by_side = TRUE, calc_length = FALSE,
                                   calc_diff = FALSE, filter_condition = NULL,
                                   transform = "No transformation",
                                   density_bw = 1, legend = TRUE, xlab = NULL) {

  if (is.null(df_rdv) || is.null(col) || nrow(df_rdv) == 0) return(NULL)

  # If the user hasn't requested cohort stratification, just create one
  if (is.null(stratify_by)) {
    stratify_by = "Cohort"
    df_rdv <- df_rdv %>% dplyr::mutate(Cohort = "All")
  }

  if (!is.null(filter_condition)) {
    df_rdv <- df_rdv %>%
      dplyr::filter(eval(rlang::parse_expr(filter_condition)))
  }

  start.time <- log_start_function_return_time("gen_distribution_plots")

  # Calculate difference between dates after grouping by date and then sum up
  # or get number of distinct dates
  if (calc_length) {
    df_rdv <- get_event_length(df_rdv, stratify_by, calc_diff)
    col <- "length"
  }

  # Make sure that the data column is fully numeric
  df_rdv[[col]] <- as.numeric(df_rdv[[col]])

  if (transform == "Log" && all(df_rdv[[col]] > 0, na.rm = TRUE)) {
      df_rdv[[col]] <- log(df_rdv[[col]])
  }

  # TODO: review modelling of repeated measurements
  df_rdv <- df_rdv %>%
    dplyr::group_by(project_id, !!as.name(stratify_by)) %>%
    dplyr::summarise(!!(col) := median(!!as.name(col), na.rm = TRUE)) %>%
    dplyr::ungroup()

  df_rdv_grouped <- get_boot_ci(df_rdv, stratify_by, col)

  # Test for demo purposes only
  rank_sum_result <- run_kw_test(df_rdv_grouped, df_rdv, stratify_by, col)

  # If there's only one value in a cohort, the plot skips them
  # This duplicated single values 2 times with a slight jitter to force plotting
  single_values <- df_rdv %>%
    dplyr::group_by(!!as.name(stratify_by)) %>%
    dplyr::count() %>%
    dplyr::filter(n == 1) %>%
    .[[1]]

  if (length(single_values) != 0) {

    df_rdv_extra <- df_rdv %>%
      dplyr::filter(!!as.name(stratify_by) %in% single_values)

    df_rdv <- dplyr::bind_rows(
      dplyr::mutate(df_rdv_extra, dplyr::across(!!as.name(col), function(x)x*0.999)),
      dplyr::mutate(df_rdv_extra, dplyr::across(!!as.name(col), function(x)x*1.001)),
      df_rdv
    )
  }

  # Plots
  xlab <- ifelse(is.null(xlab), stringr::str_to_sentence(gsub("_", " ", col)), xlab)
  density <- plot_density(
    df_plot = df_rdv, xvar = col, fill = stratify_by, colour = stratify_by,
    xlab = ifelse(side_by_side, xlab, ""),
    density_bw = density_bw,
    show.legend = legend
  )

  boxplot <- plot_boxplot(
    df_plot = df_rdv, xvar = col, yvar = stratify_by, colour = stratify_by,
    xlab = xlab
  )

  g1 <- ggplot2::ggplotGrob(density)
  g2 <- ggplot2::ggplotGrob(boxplot)

  # Align the plots if not side by side
  if (!side_by_side) {
    maxWidth = grid::unit.pmax(g1$widths[2:5], g2$widths[2:5])
    g1$widths[2:5] <- as.list(maxWidth)
    g2$widths[2:5] <- as.list(maxWidth)
  }

  if (side_by_side) {
    plot_grob <- gridExtra::arrangeGrob(g1, g2, ncol = 2)
  } else {
    plot_grob <- gridExtra::arrangeGrob(g1, g2, ncol = 1, heights = c(3,1))
  }

  names(df_rdv_grouped) <- stringr::str_to_title(gsub('[^a-zA-Z0-9]', ' ', names(df_rdv_grouped)))

  log_record_time_taken("gen_distribution_plots", start.time)

  return(list(
    test_result = rank_sum_result,
    plot = plot_grob,
    tbl_n = df_rdv_grouped
  ))


}
