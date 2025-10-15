#' Unit testing for Analysis of variability for value-based data

#' Test runs the low-level module on some test data to check correctness
test_that("run data", {

  df_rdv <- df_flw_main %>%
    dplyr::filter(type == "WEIGHT/SCALE_grams")

  mod_output <- driveanalytics::gen_value_variability_analysis(df_rdv, "value", "taken_datetime")
  testthat::expect_s3_class(mod_output, "gtable")

  # grid::grid.draw(mod_output)

})


#Test checks that we can create the shiny app and can be used for debugging
test_that("shiny app", {
  testthat::skip(message = "Not working on Gitlab CI")
  app_ui <- shiny::fluidPage(driveanalytics::gen_value_variability_analysis_ui("test"))
  app_server <- function(input, output, session) {
    rdv_in <- shiny::reactive({df_flw_main})
    driveanalytics::gen_value_variability_analysis_server("test", rdv_in, "value", "taken_datetime", "type")
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_s3_class(app, "shiny.appobj")

  # shiny::runApp(app) # Use this line to run app interactively for debugging
})


#' Tests that the module has a functional markdown file
test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  df_rdv <- df_flw_main %>%
    dplyr::filter(type == "WEIGHT/SCALE_grams")

  rmd_file <- system.file("rmd", "gen_value_variability_analysis-report.Rmd", package = "driveanalytics")

  # Markdown parameters
  render_params <- list(
    df_rdv = df_rdv,
    value_col = "value",
    datetime_col = "taken_datetime"
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
  testthat::expect_true(file.exists(result_html))
  testthat::expect_true(file.exists(result_pdf))
  # testthat::expect_snapshot_file(result_html, "report.html")
  # testthat::expect_snapshot_file(result_pdf, "report.pdf") # PDF not supported
  # testthat::expect_snapshot(file.info(result_pdf)$size)

  # browseURL(result_html) # Use this line to run app interactively for debugging
  # browseURL(result_pdf) # Use this line to run app interactively for debugging
})
