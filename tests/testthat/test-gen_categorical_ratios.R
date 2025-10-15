test_that("run data", {

  gcr <- driveanalytics::gen_categorical_ratios(df_pde, "sex_name", "cohort")

  # Types
  testthat::expect_s3_class(gcr$plt, "ggplot")
  testthat::expect_equal(gcr$test_res, "Pearson's Chi-squared test with simulated p-value\n\t (based on 2000 replicates) p-value<0.01")

  # Plot
  # vdiffr::expect_doppelganger("plt-ggplot", gcr$plt)
})


test_that("shiny", {

  app_ui <- shiny::fluidPage(driveanalytics::gen_categorical_ratios_ui("test"))
  app_server <- function(input, output, session) {
    pde_in <- shiny::reactive({df_pde})
    driveanalytics::gen_categorical_ratios_server("test", pde_in, "sex_name", "cohort")
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_equal(class(app), "shiny.appobj")

  # shiny::runApp(app) # Use this line to run app interactively for debugging
})


test_that("report generation as HTML", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_categorical_ratios-report.Rmd", package = "driveanalytics")
  output_dir <- withr::local_tempdir(clean = FALSE)

  render_params <- list(
    input_name = "Sex",
    df_rdv = df_pde,
    event_col = "sex_name",
    stratify_by = "cohort"
  )

  # HTML report
  result_html <- rmarkdown::render(
    input = rmd_file,
    output_file = output_dir,
    params = render_params
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
