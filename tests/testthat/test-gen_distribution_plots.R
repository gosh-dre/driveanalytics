test_that("run data", {

  gdp <- driveanalytics::gen_distribution_plots(
    df_rdv = df_had,
    col = "length",
    stratify_by = "cohort",
    side_by_side = TRUE,
    calc_length = TRUE,
    calc_diff = TRUE
  )

  # types

  testthat::expect_type(gdp, "list")
  testthat::expect_type(gdp$test_result, "character")
  testthat::expect_type(gdp$plot, "list")
  testthat::expect_s3_class(gdp$tbl_n, "data.frame")

})

test_that("no RDV data", {

  gdp <- driveanalytics::gen_distribution_plots(
    df_rdv = NULL,
    col = "length",
    stratify_by = "cohort",
    side_by_side = TRUE,
    calc_length = TRUE,
    calc_diff = TRUE
  )

  # types

  testthat::expect_null(gdp$test_result)
  testthat::expect_null(gdp$plot)
  testthat::expect_null(gdp$tbl_n)

  # number of plots
  testthat::expect_equal(length(gdp$plot), 0)

})

test_that("do not log transform if negative numbers", {

  # if log transformation fails use non-transformed column in analysis
  # TODO: check table
  df_flw_main_test <- df_flw_main
  df_flw_main_test$value_neg <- -df_flw_main_test$value
  gdp <- driveanalytics::gen_distribution_plots(
    df_rdv = df_flw_main_test,
    col = "value_neg",
    stratify_by = "cohort",
    side_by_side = TRUE,
    calc_length = TRUE,
    calc_diff = TRUE,
    transform = TRUE
  )

  # types

  testthat::expect_type(gdp, "list")
  testthat::expect_type(gdp$test_result, "character")
  testthat::expect_type(gdp$plot, "list")
  testthat::expect_s3_class(gdp$tbl_n, "data.frame")

})

#' Test checks that we can create the shiny app and can be used for debugging
test_that("shiny app", {

  app_ui <- shiny::fluidPage(driveanalytics::gen_distribution_plots_ui("test"))
  app_server <- function(input, output, session) {
    df_rdv_react <- shiny::reactive({df_had})
    driveanalytics::gen_distribution_plots_server(
      id = "test",
      df_rdv = df_rdv_react,
      col = "length",
      stratify_by = "cohort",
      calc_length = TRUE,
      calc_diff = TRUE
    )
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  #shiny::runApp(app)

  testthat::expect_equal(class(app), "shiny.appobj")

})

#' Tests that the module has a functional markdown file
test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_distribution_plots-report.Rmd", package = "driveanalytics")

  # Markdown parameters
  render_params <- list(
    df_rdv = df_had,
    col = "length",
    stratify_by = "cohort",
    side_by_side = TRUE,
    calc_length = TRUE,
    calc_diff = TRUE
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
