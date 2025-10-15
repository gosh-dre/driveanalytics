#' Unit testing for List patients by cohort

#' Test runs the low-level module on some test data to check correctness
test_that("run data", {

  mod_output <- driveanalytics::pde_cohort_list(df_pde)

  # Output datatable
  testthat::expect_s3_class(mod_output, "datatables")
})


#' Test checks that we can create the shiny app and can be used for debugging
test_that("shiny app", {

  app_ui <- shiny::fluidPage(driveanalytics::pde_cohort_list_ui("test"))
  app_server <- function(input, output, session) {
    df <- shiny::reactive({df_pde})
    driveanalytics::pde_cohort_list_server("test", df)
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_equal(class(app), "shiny.appobj")

  # shiny::runApp(app) # Use this line to run app interactively for debugging
})


#' Tests that the module has a functional markdown file
test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "pde_cohort_list-report.Rmd", package = "driveanalytics")

  # Markdown parameters
  render_params <- list(
    # RMD parameters go here as a named list
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
  # testthat::expect_snapshot_file(result_html, "report.html") # removing this until dummy data are approved
  # testthat::expect_snapshot_file(result_pdf, "report.pdf") # PDF not supported
  # testthat::expect_snapshot(file.info(result_pdf)$size)

  # browseURL(result_html) # Use this line to run app interactively for debugging
  # browseURL(result_pdf) # Use this line to run app interactively for debugging
})
