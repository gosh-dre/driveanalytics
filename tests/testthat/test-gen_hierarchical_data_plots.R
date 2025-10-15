test_that("run data", {
  testthat::skip(message = "Not working on Gitlab CI")
  ghdp <- driveanalytics::gen_hierarchical_data_plots(
    df_rdv = df_dia_conditions_icd10,
    group_cols = c("icd10_chapter", "icd10_section", "diag_name"),
    size_col = "project_id",
    stratify_by = "cohort"
  )

  testthat::expect_type(ghdp, "list")
  testthat::expect_true(inherits(ghdp$plt, "plotly"))

})

test_that("no RDV data", {
  testthat::skip(message = "Not working on Gitlab CI")
  ghdp <- driveanalytics::gen_hierarchical_data_plots(
    df_rdv = NULL,
    group_cols = c("icd10_chapter", "icd10_section", "diag_name"),
    size_col = "project_id",
    stratify_by = "cohort"
  )

  testthat::expect_null(ghdp)

})

test_that("invalid group column name", {
  testthat::skip(message = "Not working on Gitlab CI")
  ghdp <- driveanalytics::gen_hierarchical_data_plots(
    df_rdv = df_dia_conditions_icd10,
    group_cols = c("icd10_ch", "diag_name"),
    size_col = "project_id",
    stratify_by = "cohort"
  )

  testthat::expect_null(ghdp)

})

test_that("missing values in grouping columns", {
  testthat::skip(message = "Not working on Gitlab CI")
  df_missing <- df_dia_conditions_icd10 %>%
    dplyr::mutate(
      icd10_chapter = NA,
      diag_name = NA
    )
  ghdp <- driveanalytics::gen_hierarchical_data_plots(
    df_rdv = df_missing,
    group_cols = c("icd10_chapter", "diag_name"),
    size_col = "project_id",
    stratify_by = "cohort"
  )

  testthat::expect_null(ghdp$plt)

})

#' Test checks that we can create the shiny app and can be used for debugging
test_that("shiny app", {
  testthat::skip(message = "Not working on Gitlab CI")
  app_ui <- shiny::fluidPage(driveanalytics::gen_hierarchical_data_plots_ui("test"))
  app_server <- function(input, output, session) {
    df_rdv_react <- shiny::reactive({df_dia_conditions_icd10})
    driveanalytics::gen_hierarchical_data_plots_server(
      id = "test",
      df_server = df_rdv_react,
      group_cols = c("icd10_chapter", "icd10_section", "diag_name"),
      size_col = "project_id",
      stratify_by = "cohort"
    )
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  # shiny::runApp(app)

  testthat::expect_equal(class(app), "shiny.appobj")

})

#' Tests that the module has a functional markdown file
test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
   rmd_file <- system.file("rmd", "gen_hierarchical_data_plots-report.Rmd", package = "driveanalytics")

   # Markdown parameters
   render_params <- list(
     df_rdv = df_dia_conditions_icd10,
     group_cols = c("icd10_chapter", "icd10_section", "diag_name"),
     size_col = "project_id",
     stratify_by = "cohort"
   )
   output_dir <- withr::local_tempdir(clean = FALSE)

   # HTML report
   result_html <- rmarkdown::render(
     input = rmd_file,
     output_file = output_dir,
     params = render_params,
     output_format = rmarkdown::html_document()
   )

   # PDF report
   result_pdf <- rmarkdown::render(
     input = rmd_file,
     output_file = output_dir,
     params = render_params,
     output_format = rmarkdown::pdf_document(includes = list(in_header = tex_preamble))
   )

  # Tests
  # testthat::expect_true(file.exists(result_html))
  # testthat::expect_true(file.exists(result_pdf))

})
