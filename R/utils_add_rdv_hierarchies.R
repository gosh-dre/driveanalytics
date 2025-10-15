#' @export
add_pde_ethnicity_hierarchy <- function(df_pde) {

  ethnicity_map = c(
    "^[A-C]$" = "White",
    "^[D-G]$" = "Mixed",
    "^[HJKL]$" = "Asian or Asian British",
    "^[MNP]$" = "Black or Black British",
    "^[RS]$" = "Other Ethnic Groups",
    "^[Z]$" = "Not Stated",
    "^[A-Z]$" = "Unknown" # Not stated != unknown
  )

  df_pde %>%
    dplyr::mutate(
      ethnicity_group = stringr::str_replace_all(ethnicity_nat_code, ethnicity_map)
    ) %>%
    tidyr::replace_na(list("ethnicity_group" = "Unknown")) %>%
    dplyr::mutate(
      ethnicity_group = factor(ethnicity_group, levels = ethnicity_map)
    )

}
