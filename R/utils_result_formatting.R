head_df_cohort <- function(df, n, col, by = NULL) {

  # Input defaults
  if (is.null(by) || by == "" || by == "NA")
    by <- stringr::str_split(names(df), "\\.", simplify = T)[[2,1]]
  if (is.null(n) || n > nrow(df) || n < 1)
    n <- nrow(df)

  df_out <- df

  if (paste0(by, ".", col) %in% names(df)) {

    # Sort by all columns
    for (c in rev(names(df)[2:length(names(df))])) {
      df_out <- dplyr::arrange(df_out, desc(!!as.name(c)))
    }

    # Sort by requested column and take top
    df_out <- df_out %>%
      dplyr::arrange(., desc(!!as.name(paste0(by, ".", col)))) %>%
      dplyr::slice_head(n = n)
  }

  # Order event factor to keep ordering when plotting
  df_out$event <- factor(df_out$event, levels = df_out$event)
  df_out

}


tabulate_df_cohort <- function(df, format = NULL, cols_percent = NULL) {

  # If not DT, then manually add the units etc.
  if (is.null(format) || format != "DT") {
    df <- df %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(cols_percent), ~sprintf("%.2f %%", 100 * .x)
      ))
  }

  # If non-kable, then tidy the titles
  if (is.null(format)) {
    df <- df %>%
      dplyr::rename_with(~stringr::str_replace(.x, "\\.", " "))
  }

  # Output format
  if (is.null(format)) {
    df
  } else if (format == "DT") {

    # Build the header table for the DT with the multi-column labels
    cn <- names(df) %>% stringr::str_split("\\.")
    cn1 <- lapply(cn, function(x)x[[1]]) %>% unlist %>% unique()
    cn2 <- lapply(cn, function(x)if(length(x)>1)x[[2]]) %>% unlist
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(lapply(cn1, function(x) {
          if (x == cn1[[1]])  return(th(x, rowspan = 2))
          else                return(th(x, colspan = 2))
        })),
        tr(lapply(cn2, th))
      )
    ))

    df %>%
      DT::datatable(container = sketch, rownames = FALSE) %>%
      DT::formatPercentage(cols_percent, digits = 2) %>%
      DT::formatStyle(cols_percent, fontSize = '100%')

  } else {

    df %>%
      knitr::kable(format = format) %>%
      kableExtra::header_separate("\\.") %>%
      kableExtra::row_spec(0, bold = TRUE) %>%
      kableExtra::column_spec(seq(ncol(df)), border_left = TRUE, border_right = TRUE) %>%
      kableExtra::column_spec(1, latex_column_spec = "X") %>%
      kableExtra::column_spec(seq(2, ncol(df), 2), latex_column_spec = "s") %>%
      kableExtra::column_spec(seq(3, ncol(df), 2), latex_column_spec = "b") %>%
      kableExtra::kable_styling(latex_table_env = "tabularxfullwidth", font_size = 7)
  }
}
