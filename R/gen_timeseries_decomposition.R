#' Generic decomposition of a timeseries by Prophet
#'
#' Description
#'
#'
#' @return DESCRIPTION.
#'
#' @family gen_timeseries_decomposition
#' @export
gen_timeseries_decomposition <- function(df_events, label = "Events") {

  start.time <- log_start_function_return_time("gen_timeseries_decomposition")

  bin_size <- "week"
  unit_lbl <- paste(label, "per", bin_size)

  # Bin events
  df_ts <- df_events %>%
    dplyr::transmute(
      ds = lubridate::floor_date(start_datetime, unit = bin_size)
    ) %>%
    dplyr::count(ds, name = "y") %>%
    dplyr::full_join(
      tibble::tibble(ds = seq(min(.$ds), max(.$ds), by = bin_size)), by = "ds"
    ) %>%
    tidyr::replace_na(list(n = 0)) %>%
    dplyr::arrange(ds)

  # Model bin'd values
  mdl_prophet <- prophet::prophet(
    df_ts,
    seasonality.mode = "multiplicative",
    interval.width = 0.95,
    yearly.seasonality = 5, #TRUE, #4, #TRUE, #5,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    growth = 'linear', #'logistic', #linear', #'flat',
    # changepoint.prior.scale = 0.002,
    # changepoint.range = 1.0,
    # seasonality.prior.scale = 0.01,
    # changepoints = c(epic_gosh_introduction, epic_gosh_introduction+90),
    # n.changepoints = 5
    mcmc.samples = 0
    # uncertainty.samples = 10000
  )

  # Forecast
  future <- prophet::make_future_dataframe(mdl_prophet, 365, 'day')
  forecast <- predict(mdl_prophet, future)

  # Plots
  plt_ts <- plot(mdl_prophet, forecast, xlabel = "Date", ylabel = unit_lbl)
  plt_components <- prophet::prophet_plot_components(mdl_prophet, forecast, render_plot = FALSE)
  plt_trend <- plt_components[[1]]
  plt_yearly <- plt_components[[2]]

  # Remove theme from plt_ts
  plt_ts$theme <- NULL
  plt_trend <- plt_trend +
    ggplot2::xlab("Date") +
    ggplot2::ylab(paste0("Trend\n(", unit_lbl, ")"))
  plt_yearly <- plt_yearly +
    ggplot2::ylab("Yearly")

  log_record_time_taken("gen_timeseries_decomposition", start.time)

  # Combined plots for output
  gb1 <- ggplot2::ggplotGrob(plt_ts)
  gb2 <- ggplot2::ggplotGrob(plt_trend)
  gb3 <- ggplot2::ggplotGrob(plt_yearly)
  gg <- gridExtra::gtable_rbind(gb1, gb2, gb3, size = "last")
  gridExtra::grid.arrange(gg)

}
