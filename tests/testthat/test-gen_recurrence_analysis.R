#' Unit testing for Analysis of the periodicity of event recurrence
date = Sys.Date()

t_data <- data.frame(
  project_id = c(1,1,2,2,3,3),
  proc_local_code = c("a","a","b","b","c","d"),
  start_datetime = c(date, (date - 1), (date - 2), (date - 2), date, date)
)

#' #' Test runs the low-level module on some test data to check correctness
test_that("run data", {
  gra <- driveanalytics::gen_recurrence_analysis(df_pro, "proc_local_code")
  expect_s3_class(gra, "gen_recurrence_analysis")
})

test_that("calculating frequency analysis", {

  ## test output is as expected
  gra <- driveanalytics::gen_recurrence_analysis(t_data, "proc_local_code")
  frequencies <- dplyr::pull(gra, time_between)
  expected_vector = c(NA, 1, NA, 0, NA, NA)
  # Output data
  testthat::expect_equal(frequencies,expected_vector)

  ## test that the same date is 0
  gra <- driveanalytics::gen_recurrence_analysis(t_data[3:4,], "proc_local_code")
  frequencies <- dplyr::pull(gra, time_between)
  expected_vector = c(NA, 0)
  # Output data
  testthat::expect_equal(frequencies,expected_vector)

  ## test 1 row
  gra <- driveanalytics::gen_recurrence_analysis(t_data[1,], "proc_local_code")
  frequencies <- dplyr::pull(gra, time_between)
  # Output data

})

test_that("summary database",{

  # test table formed correctly
  expected_df <- data.frame(
    names = c("a","b"),
    mean_days_all = c(1,0),
    median_days_all = c(1,0),
    range_all = c(0,0),
    total_events_all = c(1,1)
  )
  expected_df <- dplyr::as_tibble(expected_df)

  gra <- driveanalytics::gen_recurrence_analysis(t_data, "proc_local_code")
  tbl <- tabulate.gen_recurrence_analysis(gra)

  # Output data
  testthat::expect_equal(tbl,expected_df)

  # test NA table formed correctly
  expected_df <- data.frame(
    proc_local_code = c("a","b"),
    mean_time_between_all = c(1,0),
    median_time_between_all = c(1,0),
    range_all = c(0,0),
    total_events_all = c(1,1)
  )
  expected_df <- dplyr::as_tibble(expected_df)

  gra <- driveanalytics::gen_recurrence_analysis(t_data[5:6,], "proc_local_code")
  tbl <- tabulate.gen_recurrence_analysis(gra)
  # Output data
  testthat::expect_equal(dim(tbl),c(0, 1))

})

test_that("plot", {
  gra <- driveanalytics::gen_recurrence_analysis(t_data, "proc_local_code")
  plt <- plot.gen_recurrence_analysis(gra, c("a","b"), FALSE)
  testthat::expect_s3_class(plt, "ggplot")
  # vdiffr::expect_doppelganger("plt-ggplot", plt)

})

test_that("shiny app", {

  app_ui <- shiny::fluidPage(driveanalytics::gen_recurrence_analysis_ui("test"))
  app_server <- function(input, output, session) {
    rdv_in <- shiny::reactive({df_pro})
    driveanalytics::gen_recurrence_analysis_server("test", rdv_in, "proc_local_code", "cohort")#, "U201")
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  # shiny::runApp(app)

  testthat::expect_equal(class(app), "shiny.appobj")
})


#' Tests that the module has a functional markdown file
test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  # rmd_index <- system.file("rmd", "index.Rmd", package = "driveanalytics")
  rmd_file <- system.file("rmd", "gen_recurrence_analysis-report.Rmd", package = "driveanalytics")

  # Markdown parameters
  render_params <- list(
    input_name = "Event recurrence",
    df_rdv = df_pro,
    colm = "proc_local_code",
    event_of_interest = "U201",
    cohort_col = "cohort"

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

  #     # Tests
  testthat::expect_true(file.exists(result_html))
  testthat::expect_true(file.exists(result_pdf))
})
