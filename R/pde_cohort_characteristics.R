#' Calculate, tabulate and format the demographic characteristics by cohort
#'
#' Description
#'
#' @param df_pde dataframe.
#'
#' @return dataframe.
#'
#' @family pde_cohort_characteristics
#' @export
pde_cohort_characteristics <- function(df_pde) {

  if (is.null(df_pde)) return(NULL)

  df <- df_pde %>%
    add_pde_ethnicity_hierarchy() %>%
    dplyr::mutate(
      age_at_entry = lubridate::time_length(as.Date(cohort_entry_date) - birth_date, "years")
    ) %>%
    dplyr::select(project_id, cohort, sex_name, ethnicity_group, age_at_entry) %>%
    dplyr::group_by(cohort) %>%
    dplyr::group_modify(.single_cohort_characteristics) %>%
    tidyr::pivot_wider(names_from = cohort, values_from = Value, values_fill = "0 %")

  # Set S3 class and return
  attr(df, "class") <- c("pde_cohort_characteristics", class(df))
  return(df)
}


#' @export
tabulate.pde_cohort_characteristics <- function(df, format = NULL) {

  # Add in the header rows with empty string values
  header_row <- df[1,] %>% dplyr::mutate(dplyr::across(dplyr::everything(), function(x){""}))

  # Order grouping factors to avoid them being put alphabetically
  df$grouping <- factor(df$grouping, levels = unique(df$grouping))

  # Add headers
  df_out <- do.call(
    rbind,
    c(
      make.row.names = FALSE,
      lapply(
        split(df, df$grouping),
        function(x) {
          if (x$grouping[[1]] != "")
            rbind(dplyr::mutate(header_row, Characteristic = x$grouping[[1]]), x)
          else
            x
        }
      )
    )) %>%
    dplyr::select(!grouping)

  # Get the column numbers
  title_cols <- which(df_out[,2] == "")

  # Output formatting
  if (is.null(format)) {
    df_out
  } else if (format == "docx") {

    df_out %>%
      flextable::flextable() %>%
      flextable::merge_h_range(i = title_cols, j1 = 1, j2 = ncol(df_out)) %>%
      flextable::bold(part = "header") %>%
      flextable::bold(i = title_cols) %>%
      flextable::autofit()
  } else {
    df_out %>%
      knitr::kable(format = format) %>%
      kableExtra::row_spec(title_cols, bold = TRUE) %>%
      kableExtra::kable_styling(full_width = TRUE)
  }

}


#' Generate the summary characteristics for a patient demographics dataframe.
#' Should be improved to work across groups.
.single_cohort_characteristics <- function(df, unused) {

  frmt_percents <- function(x) paste( format(round(x, 2), nsmall=2), "%")
  frmt_years <- function(x) paste( format(round(x, 2), nsmall=2), "years")
  frmt_value_percents <- function(x) dplyr::mutate(x, Value = frmt_percents(Value))
  frmt_value_years <- function(x) dplyr::mutate(x, Value = frmt_years(Value))

  # Relabel unknown values
  df <- df %>%
    replace(is.na(.), "Unknown")

  df_pde_pop <- dplyr::bind_rows(
    dplyr::summarise(df, grouping = "N", Characteristic = "Periods", Value = paste(dplyr::n())),
    dplyr::summarise(df, grouping = "N", Characteristic = "Patients", Value = paste(length(unique(project_id))))
  )

  df_pde_sex <- df %>%
    dplyr::summarise(
      Female = 100 * sum(sex_name == "Female") / dplyr::n(),
      Male = 100 * sum(sex_name == "Male") / dplyr::n(),
      `Indeterminate / Unspecified` = 100 - Female - Male
    ) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::rename(Characteristic = name, Value = value) %>%
    frmt_value_percents() %>%
    dplyr::mutate(grouping = "Sex")

  df_pde_eth <- as.data.frame(table(df$ethnicity_group)) %>%
    dplyr::mutate(Freq = 100 * Freq / sum(Freq)) %>%
    dplyr::rename(Characteristic = Var1, Value = Freq) %>%
    dplyr::arrange(desc(Value)) %>%
    frmt_value_percents() %>%
    dplyr::mutate(grouping = "Ethnic Category")

  df_pde_age <- df %>%
    dplyr::summarise(
      Minimum = max(0, min(age_at_entry)),
      `25th Percentile` = quantile(age_at_entry)[[2]],
      Median = median(age_at_entry),
      `75th Percentile` = quantile(age_at_entry)[[4]],
      Maximum = max(age_at_entry)
    ) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::rename(Characteristic = name, Value = value) %>%
    frmt_value_years() %>%
    dplyr::mutate(grouping = "Age at Entry to Cohort")

  df_pde_all <- dplyr::bind_rows(df_pde_pop, df_pde_sex, df_pde_eth, df_pde_age)


  return(df_pde_all)
}
