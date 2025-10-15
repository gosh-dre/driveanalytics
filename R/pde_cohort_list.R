#' List patients by cohort
#'
#' Description
#'
#'
#' @return DESCRIPTION.
#'
#' @family pde_cohort_list
#' @export
pde_cohort_list <- function(df_pde) {

  df_cl <- df_pde %>%
    dplyr::select(
      tidyselect::any_of(c(
        # "hospital_no",
        "surname", "forename", "birth_date", "death_date"
      )),
      dplyr::starts_with("cohort")
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_date"), as.Date))

  df_cl %>%
    DT::datatable(
      colnames = names(df_cl) %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_title(),
      rownames = FALSE
    ) %>%
    DT::formatDate(which(stringr::str_detect(names(df_cl), "_date$")))

}
