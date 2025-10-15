#' Generate plots for hierarchical data
#'
#' Output: a treemap
#'
#' @param df_rdv Dataframe RDV.
#' @param group_cols Character; name of columns in df_rdv represented by a tile
#' ordered from higher to lower level.
#' @param size_col Character; name of column in df_rdv to use for count.
#' @param stratify_by Character; name of column in df_rdv to use for other grouping.
#'
#' @return A list of output plots.
#'
#' @family gen_hierarchical_data_plots
#' @export
gen_hierarchical_data_plots <- function(df_rdv, group_cols, size_col, stratify_by = NULL) {

  # Ensure input is unnamed vector of values
  group_cols <- unname(unlist(group_cols))

  if (is.null(df_rdv) || nrow(df_rdv) == 0 || !all((c(group_cols, size_col) %in% names(df_rdv)))) return(NULL)

  if (is.null(stratify_by)) {
    stratify_by <- "cohort"
    df_rdv[[stratify_by]] <- "All patients"
  }

  logger::log_info("Started gen_hierarchical_data_plots")
  start.time <- Sys.time()

  list_df <- split(df_rdv, df_rdv[[stratify_by]])
  list_df[sapply(list_df, function(x) is.null(x) | nrow(x) == 0)] <- NULL

  # prepare data

  list_df_tree <- lapply(list_df, function(x) {

    df <- x %>%
      dplyr::filter(if_all(tidyselect::all_of(group_cols), ~ !is.na(.)))
    if (nrow(df) == 0) return(NULL)
    df <- df %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) %>%
      dplyr::summarise(size = dplyr::n_distinct(!!as.name(size_col))) %>%
      dplyr::ungroup() #%>%
      #dplyr::slice_max(size, n = 10)

    # convert to a tree structure
    df$all <- paste("All in", unique(x[[stratify_by]]))
    df$groups <- do.call(paste, c(df[group_cols], sep = "/"))
    df$pathString <- paste(df$all, df$groups, sep = "/")

    data_tree <- data.tree::FromDataFrameTable(df, pathName = "pathString")

    # total for each level
    data_tree$Do(function(node)
      node$size <- data.tree::Aggregate(node, attribute = "size", aggFun = sum),
      traversal = "post-order")
    #data_tree <- as.Node(df)
    df_tree <- data.tree::ToDataFrameNetwork(data_tree, "size")
    df_tree$parents <- df_tree$from
    df_tree$labels <- df_tree$to

    df_tree

  })

  names(list_df_tree) <- names(list_df)

  # Attempted to use subplot with a list of plotly treemaps but didn't work

  treemap <- NULL

  if (length(list_df_tree) > 0) {
    if (!is.null(list_df_tree[[1]])) {
      treemap <- plotly::plot_ly(type = "treemap",
                                 branchvalues = "total",
                                 values = list_df_tree[[1]]$size,
                                 labels = list_df_tree[[1]]$labels,
                                 parents = list_df_tree[[1]]$parents,
                                 textinfo = "label+value+percent parent+percent root",
                                 domain = list(column = 0))
    }
  }

  if (length(list_df_tree) > 1) {
    for (i in 2:length(list_df_tree)) {
      if (!is.null(list_df_tree[[i]]) && nrow(list_df_tree[[i]]) > 0) {
        treemap <- treemap %>%
          plotly::add_trace(inherit = FALSE,
                            type = "treemap",
                            branchvalues = "total",
                            values = list_df_tree[[i]]$size,
                            labels = list_df_tree[[i]]$labels,
                            parents = list_df_tree[[i]]$parents,
                            textinfo = "label+value+percent parent+percent root",
                            domain = list(column = i-1))
      }
    }
  }

  list_df_tree <- Filter(Negate(is.null), list_df_tree)

  if (length(list_df_tree) > 0) {
    treemap <- treemap %>%
      plotly::layout(grid = list(columns = length(list_df_tree), rows = 1))
  }

  log_record_time_taken("gen_hierarchical_data_plots", start.time)

  list(plt = treemap)

}
