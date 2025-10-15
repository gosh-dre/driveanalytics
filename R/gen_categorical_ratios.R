#' Simple analysis of the ratios of categorical variables within an RDV
#'
#' Description
#'
#' @param df data frame to plot
#' @param col column of data frame to analyse
#' @param stratify_by column of data frame to categorise the data cohort comparison
#'
#' @return ggplot of categorical splits
#'
#' @family gen_categorical_ratios
#' @export
gen_categorical_ratios <- function(df, col, stratify_by = NULL) {

  if (is.null(df) || is.null(col)) return(NULL)

  # If the user hasn't requested cohort stratification, just create one
  if (is.null(stratify_by)) {
    stratify_by = "Cohort"
    df <- df %>% dplyr::mutate(Cohort = "All")
  }

  logger::log_info("Started gen_categorical_ratios")
  start.time <- Sys.time()

  df_tab <- df %>%
    dplyr::group_by(!!as.name(stratify_by), !!as.name(col)) %>%
    dplyr::count()

  plt_cat_splits <- ggplot2::ggplot(df_tab, ggplot2::aes(x = !!as.name(stratify_by), y = n, fill = !!as.name(col))) +
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::ylab("Fraction of Cohort")

  # Run test only for 2 groups with 0 IDs in common and count >=5 per cell
  # TODO: Prevent test from running if group selection (stratify_by) is based on col
  # Warning: Test results should not be interpreted, test is for demo purposes only

  test_res <- NULL

  # is df_tab$n number of patients?
  if (length(Reduce(intersect, split(df$project_id, df[[stratify_by]]))) == 0 &
      length(unique(df[[stratify_by]])) == 2 &
      all(df_tab$n >= 5)) {
    test <- chisq.test(df[[stratify_by]], df[[col]], simulate.p.value = TRUE)
    pval <- ifelse(
      test$p.value < 0.01, "<0.01",
      ifelse(
        test$p.value > 0.9, ">0.9",
        paste0("=", round(test$p.value, 2))
      )
    )
    test_res <- paste0(test$method, " p-value", pval)
  }

  log_record_time_taken("gen_categorical_ratios", start.time)

  list("plt" = plt_cat_splits,
       "test_res" = test_res)

}
