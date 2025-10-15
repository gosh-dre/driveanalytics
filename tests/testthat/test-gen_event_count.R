test_that("run data", {

  n <- 20

  gec <- gen_event_count(df_dia, df_pde, "diag_name")

  plt <- plot(gec)
  tbl <- tabulate(gec)
  tbl_latex <- tabulate(gec, format = "latex")
  tbl_dt <- tabulate(gec, format = "DT")

  # Results (sample random rows)
  # TODO

  # Plot
  testthat::expect_s3_class(plt, "ggplot")
  # vdiffr::expect_doppelganger("plt-ggplot", plt)

  # Formatted table
  testthat::expect_s3_class(tbl, "tbl_df")
  # testthat::expect_snapshot(tbl)

  # Latex table
  testthat::expect_s3_class(tbl_latex, "knitr_kable")
  # testthat::expect_snapshot(tbl_latex)

  # DT table
  testthat::expect_s3_class(tbl_dt, "datatables")
  tbl_dt_list <- tbl_dt
  attr(tbl_dt_list, "class") <- "list"
  tbl_dt_list$preRenderHook <- NULL
  # testthat::expect_snapshot(tbl_dt_list)

})


test_that("shiny app", {

  app_ui <- shiny::fluidPage(driveanalytics::gen_event_count_ui("test"))
  app_server <- function(input, output, session) {
    rdv_in <- shiny::reactive({df_dia})
    pde_in <- shiny::reactive({df_pde})
    driveanalytics::gen_event_count_server("test", rdv_in, pde_in, "diag_name", "cohort")
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_equal(class(app), "shiny.appobj")

  # shiny::runApp(app) # Use this line to run app interactively for debugging
})


test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_event_count-report.Rmd", package = "driveanalytics")

  # Markdown parameters
  render_params <- list(
    input_name = "ICD-10 Coded Co-morbidities",
    df = df_dia,
    df_pde = df_pde,
    event_col = "diag_name"
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
