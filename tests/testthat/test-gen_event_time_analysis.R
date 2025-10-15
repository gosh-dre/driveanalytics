test_that("run data", {

  geta <- driveanalytics::gen_event_time_analysis(
    df_rdv = df_had,
    event_col = "admission_type",
    stratify_by = "cohort",
    plot_type = "Boxplot",
    plot_options = FALSE
  )

  testthat::expect_type(geta, "list")
  testthat::expect_s3_class(geta$df, "data.frame")
  testthat::expect_s3_class(geta$plt, "ggplot")

})

test_that("missing input", {

  geta <- driveanalytics::gen_event_time_analysis(
    df = NULL,
    event_col = "admission_type",
    stratify_by = "cohort",
    plot_type = "Boxplot",
    plot_options = FALSE
  )

  testthat::expect_null(geta$df)
  testthat::expect_null(geta$plt)

  geta <- driveanalytics::gen_event_time_analysis(
    df_rdv = df_had,
    event_col = NULL,
    stratify_by = "cohort",
    plot_type = "Boxplot",
    plot_options = FALSE
  )

  testthat::expect_null(geta$df)
  testthat::expect_null(geta$plt)

})

test_that("NULL stratify_by", {

  geta <- driveanalytics::gen_event_time_analysis(
    df_rdv = df_had,
    event_col = "admission_type",
    stratify_by = NULL,
    plot_type = "Boxplot",
    plot_options = FALSE
  )
  # testthat::expect_equal(ncol(geta$df), 6)
  testthat::expect_equal(nrow(geta$df), length(unique(df_had$admission_type)))
  testthat::expect_true(!is.null(geta$plt))

})


test_that("correct summary table", {

  geta <- driveanalytics::gen_event_time_analysis(
    df = df_had,
    event_col = "admission_type",
    stratify_by = "cohort",
    plot_type = "Boxplot",
    plot_options = FALSE
  )

  # testthat::expect_equal(nrow(geta$df), length(unique(df_had$admission_type)))
  # testthat::expect_equal(ncol(geta$df), 5 * length(unique(df_had$cohort)) + 1)
  testthat::expect_true(all(unique(geta$df$event) %in% unique(df_had$admission_type)))

})

#' Test checks that we can create the shiny app and can be used for debugging
test_that("shiny app", {

  app_ui <- shiny::fluidPage(driveanalytics::gen_event_time_analysis_ui("test"))
  app_server <- function(input, output, session) {
    df_rdv_react <- shiny::reactive({df_had})
    driveanalytics::gen_event_time_analysis_server(
      id = "test",
      event_col = "admission_type",
      stratify_by = "cohort",
      df_server = df_rdv_react
    )
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  # shiny::runApp(app)

  testthat::expect_equal(class(app), "shiny.appobj")

})

#' Tests that the module has a functional markdown file
test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_event_time_analysis-report.Rmd", package = "driveanalytics")

  # Markdown parameters
  render_params <- list(
    df = df_had,
    event_col = "admission_type",
    stratify_by = "cohort",
    plot_type = "Boxplot",
    plot_options = FALSE
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

})
