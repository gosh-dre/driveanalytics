#' @export
tpl_pde_all_ui <- function(id) {

  ns <- shiny::NS(id)

  list(
    shiny::tabPanel("Overview", pde_cohort_characteristics_ui(ns("tpl_pde_all_overview"))),
    shiny::tabPanel("Patient List", pde_cohort_list_ui(ns("tpl_pde_all_patients"))),
    shiny::tabPanel("Sex", gen_categorical_ratios_ui(ns("tpl_pde_all_sex"))),
    shiny::tabPanel("Ethnicity", gen_categorical_ratios_ui(ns("tpl_pde_all_ethnicity"))),
    shiny::tabPanel("Age at Cohort Entry", gen_distribution_plots_ui(ns("tpl_pde_all_age")))
  )

}


#' @export
tpl_pde_all_server <- function(id, df_pde, df_loc) {

  ns <- shiny::NS(id)

  # Derived RDVs
  df_pde_age_at_cohort_entry <- gen_age_at_event_server("age_at", df_pde, "cohort_entry_date")

  # Server functions
  pde_cohort_characteristics_server(ns("tpl_pde_all_overview"), df_pde)
  pde_cohort_list_server(ns("tpl_pde_all_patients"), df_pde)
  gen_categorical_ratios_server(ns("tpl_pde_all_sex"), df_pde, "sex_name", "cohort")
  gen_categorical_ratios_server(ns("tpl_pde_all_ethnicity"), df_pde, "ethnicity_name", "cohort")
  gen_distribution_plots_server(ns("tpl_pde_all_age"), df_pde_age_at_cohort_entry, "age_years_at_cohort_entry_date", "cohort")

}


#' @export
tpl_pde_all_report <- function(df_pde, df_loc) {

  rmd_pde_cohort_characteristics <- system.file("rmd", "pde_cohort_characteristics-report.Rmd", package = "driveanalytics")
  rmd_gen_categorical_ratios <- system.file("rmd", "gen_categorical_ratios-report.Rmd", package = "driveanalytics")
  rmd_gen_distribution_plots <- system.file("rmd", "gen_distribution_plots-report.Rmd", package = "driveanalytics")

  cfg <- list(
    list(
      rmd_file = rmd_pde_cohort_characteristics,
      df_pde = df_pde()
    ),
    list(
      rmd_file = rmd_gen_categorical_ratios,
      input_name = "Biological Sex",
      df_rdv = df_pde(),
      event_col = "sex_name",
      stratify_by = "cohort"
    ),
    list(
      rmd_file = rmd_gen_categorical_ratios,
      input_name = "Ethnicity",
      df_rdv = df_pde(),
      event_col = "ethnicity_name",
      stratify_by = "cohort"
    ),
    list(
      rmd_file = rmd_gen_distribution_plots,
      df_rdv = add_age_at_event(df_pde(), "cohort_entry_date"),
      col = "age_years_at_cohort_entry_date",
      stratify_by = "cohort",
      description = "Age at Cohort Entry"
    )
  )

  return(cfg)
}
