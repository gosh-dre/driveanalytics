#' Unit testing for Generic decomposition of a timeseries by Prophet

#' Test runs the low-level module on some test data to check correctness
test_that("run data", {

  mod_output <- driveanalytics::gen_timeseries_decomposition(df_wst, "PICU admissions")
  testthat::expect_s3_class(mod_output, "gtable")

})


#' Test checks that we can create the shiny app and can be used for debugging
test_that("shiny app", {
  app_ui <- shiny::fluidPage(driveanalytics::gen_timeseries_decomposition_ui("test"))
  app_server <- function(input, output, session) {
    df <- shiny::reactive({df_wst})
    driveanalytics::gen_timeseries_decomposition_server("test", df)
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_s3_class(app, "shiny.appobj")

  #shiny::runApp(app) # Use this line to run app interactively for debugging
})


#' Tests that the module has a functional markdown file
test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_timeseries_decomposition-report.Rmd", package = "driveanalytics")

  # Markdown parameters
  render_params <- list(
    df_rdv = df_wst,
    title = "PICU admissions"
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

  # browseURL(result_html)
  # browseURL(result_pdf)
})
