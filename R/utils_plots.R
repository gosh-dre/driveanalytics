#' @export
plot_density <- function(df_plot,
                         xvar,
                         fill,
                         colour,
                         xlab = NULL,
                         density_bw = 1,
                         show.legend = TRUE) {
  plt <- df_plot %>%
    dplyr::filter(!is.na(!!as.name(xvar))) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = !!as.name(xvar),
      fill = !!as.name(fill),
      colour = !!as.name(colour)
    )) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab("") +
    ggplot2::geom_density(
      ggplot2::aes(y = ggplot2::after_stat(scaled)),
      alpha = 0.5,
      adjust = density_bw,
      show.legend = show.legend
    ) +
    ggplot2::theme_minimal()

  return(plt)

}

#' @export
plot_boxplot <- function(df_plot, xvar, yvar, colour, xlab) {
  plt <- ggplot2::ggplot(df_plot,
                         ggplot2::aes(
                           x = !!as.name(xvar),
                           y = !!as.name(yvar),
                           colour = !!as.name(colour)
                         )) +
    ggplot2::geom_boxplot(varwidth = TRUE,
                          outlier.shape = NA,
                          na.rm = TRUE) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  return(plt)

}

# Function code from https://stackoverflow.com/questions/25124895/no-outliers-in-ggplot-boxplot-with-facet-wrap#:~:text=It%20can%20be,%22free%22)
# Function that calculates Box Plot statistics
get_boxplot_stats <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  stats <- quantile(x)
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  outliers <- x < (stats[2] - coef * iqr) |
    x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

#' @export
get_event_length <- function(df_rdv, stratify_by, calc_diff) {
  if (calc_diff) {
    df_rdv <- df_rdv %>%
      dplyr::mutate(
        end_datetime = dplyr::case_when(
          start_datetime >= end_datetime ~ start_datetime,
          is.na(end_datetime) ~ start_datetime,
          .default = end_datetime
        )
      )
    df_rdv$start_date <- as.Date(df_rdv$start_datetime)
    df_rdv$end_date <- as.Date(df_rdv$end_datetime)
    df_rdv <- df_rdv %>%
      dplyr::group_by(project_id,!!as.name(stratify_by),
                      row_num = dplyr::row_number())
    lst <- Map(function(x, y)
      seq(x, y, by = "1 day"),
      df_rdv$start_date,
      df_rdv$end_date)
    i1 <- rep(1:nrow(df_rdv), lengths(lst))
    df_rdv <- data.frame(df_rdv[i1, -3], all_dates = do.call("c", lst))
    df_rdv <- df_rdv %>%
      dplyr::group_by(project_id, !!as.name(stratify_by)) %>%
      dplyr::summarise(length = dplyr::n_distinct(all_dates)) %>%
      dplyr::ungroup()
  } else {
    df_rdv <- df_rdv %>%
      dplyr::group_by(project_id, !!as.name(stratify_by)) %>%
      dplyr::summarise(length = dplyr::n_distinct(as.Date(start_datetime))) %>%
      dplyr::ungroup()
  }
  return(df_rdv)
}

#' @export
get_boot_ci <- function(df_rdv, stratify_by, col) {
  # boot::boot.ci sometimes generates messages that break the rmd
  # This needs a more robust solution
  capture.output({
    list_df_rdv <- split(df_rdv, df_rdv[[stratify_by]])
    list_df_boot_ci <- list()
    for (i in 1:length(list_df_rdv)) {
      median <- median(list_df_rdv[[i]][[col]], na.rm = TRUE)
      bootstrap_confidence_interval_95 <- tryCatch({
        boot_res <- boot::boot.ci(boot::boot(
          list_df_rdv[[i]][[col]],
          statistic = function(x, j)
            median(x[j], na.rm = TRUE),
          R = 1000
        ),
        type = "basic")
        paste0("[",
               format(round(boot_res$basic[4], 2), nsmall = 2),
               ",",
               format(round(boot_res$basic[5], 2), nsmall = 2),
               "]")
      }, error = function(e) {
        return(NA)
      })
      df_temp <- NULL
      if ("project_id" %in% names(list_df_rdv[[i]])) {
        patient_count <- dplyr::n_distinct(list_df_rdv[[i]]["project_id"])
        df_temp <- data.frame(group = unique(names(list_df_rdv)[[i]]),
                              patient_count,
                              median,
                              bootstrap_confidence_interval_95)
      } else {
        df_temp <- data.frame(group = unique(names(list_df_rdv)[[i]]),
                              median,
                              bootstrap_confidence_interval_95)
      }
      list_df_boot_ci[[i]] <- df_temp
    }
    df_rdv_grouped <- do.call(rbind, list_df_boot_ci)
  }, file = nullfile())

  return(df_rdv_grouped)

}

#' @export
run_kw_test <- function(df_rdv_grouped, df_rdv, stratify_by, col) {
  if (any(df_rdv_grouped$patient_count < 5)) {
    return("No test was run due to low patient count")
  }
  if (length(unique(df_rdv[[stratify_by]])) < 2) {
    return("Test requires at least 2 groups")
  }

  df_list <- split(df_rdv, df_rdv[[stratify_by]])
  common <- Reduce(intersect, lapply(df_list, function(x)
    unique(x[["project_id"]])))
  if (length(common) > 0) {
    return("Test requires independent groups")
  }

  rank_sum_test <- kruskal.test(df_rdv[[col]] ~ df_rdv[[stratify_by]], data = df_rdv)
  pval <- ifelse(
    rank_sum_test$p.value < 0.01,
    "<0.01",
    ifelse(rank_sum_test$p.value > 0.9, ">0.9", paste0("=", round(
      rank_sum_test$p.value, 2
    )))
  )
  return(rank_sum_result <- paste0(rank_sum_test$method, ": p-value", pval))

}
