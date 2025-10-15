test_that("run data", {

  n <- 20

  gfa <- gen_frequency_analysis(df_dia, df_pde, "diag_name", "cohort")
  gfa_top <- head(gfa, n, "A")

  plt <- plot(gfa_top)
  tbl <- tabulate(gfa_top)
  tbl_latex <- tabulate(gfa_top, format = "latex")
  tbl_dt <- tabulate(gfa_top, format = "DT")

  # Results (sample random rows)
  check_result <- function(x) {
    cohorts <- df_dia %>%
      dplyr::filter(diag_name %in% x[["event"]]) %>%
      dplyr::group_by(project_id) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::pull(cohort)
    testthat::expect_equal(
      as.numeric(x[["A.count"]]), sum(cohorts == "A"),
      label = paste("Cohort A is wrong for", x[["event"]])
    )
    testthat::expect_equal(
      as.numeric(x[["B.count"]]), sum(cohorts == "B"),
      label = paste("Cohort B is wrong for", x[["event"]])
    )
  }
  dplyr::sample_n(gfa, 30) %>%
    dplyr::arrange(dplyr::desc("A.count")) %>%
    apply(1, check_result)

  # Output data
  testthat::expect_equal(
    nrow(gfa_top), n,
    label = "Table head should be only top n"
  )
  testthat::expect_equal(
    gfa_top$`A.count`, sort(gfa_top$`A.count`, decreasing = TRUE),
    label = "Table head should be ordered by cohort A"
  )
  testthat::expect_equal(
    gfa_top$`A.frequency`, sort(gfa_top$`A.frequency`, decreasing = TRUE),
    label = "Table head should be ordered by cohort A"
  )

  # Plot
  testthat::expect_s3_class(plt, "ggplot")
  # vdiffr::expect_doppelganger("plt-ggplot", plt)

  # Formatted table
  testthat::expect_s3_class(tbl, "tbl_df")
  # testthat::expect_snapshot(tbl)
  testthat::expect_equal(attr(tbl, "totals")[["event"]], "TOTAL")
  # testthat::expect_equal(attr(tbl, "totals")[["A.count"]], 6480)
  # testthat::expect_equal(attr(tbl, "totals")[["A.frequency"]], 24.3, tolerance = 0.1)
  # testthat::expect_equal(attr(tbl, "totals")[["B.count"]], 5011)
  # testthat::expect_equal(attr(tbl, "totals")[["B.frequency"]], 21.5, tolerance = 0.1)

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

  app_ui <- shiny::fluidPage(driveanalytics::gen_frequency_analysis_ui("test"))
  app_server <- function(input, output, session) {
    rdv_in <- shiny::reactive({df_dia})
    pde_in <- shiny::reactive({df_pde})
    driveanalytics::gen_frequency_analysis_server("test", rdv_in, pde_in, "diag_name", "cohort")
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_s3_class(app, "shiny.appobj")

  # shiny::runApp(app) # Use this line to run app interactively for debugging
})


test_that("shiny server", {

  # To test shiny, we can test the server functionality, but not the UI yet
  n <- 20

  # Testing parameters
  category = "diag_name"

  # Load the 'true' values from the raw function
  gfa_extern <- driveanalytics::gen_frequency_analysis(df_dia, df_pde, category, "cohort")
  gfa_hd_extern <- head(gfa_extern, n)
  plt_extern <- plot(gfa_hd_extern)
  tbl_extern <- tabulate(gfa_hd_extern)

  # Test the shiny server module
  shiny::testServer(
    driveanalytics::gen_frequency_analysis_server,
    args = list(
      id = "test",
      df_rdv_server = shiny::reactive({df_dia}),
      df_pde_server = shiny::reactive({df_pde}),
      event_col = "diag_name",
      cohort_col = "cohort"
    ),
    {
      # It is necessary to set at least one input to get this to work
      session$setInputs(category = category, nplot = n)

      # Standard testthat tests on module reactives/outputs
      testthat::expect_equal(gfa(), gfa_extern)
      # testthat::expect_snapshot(output$table) # If this line throws, it's failed
      # testthat::expect_snapshot(output$plot) # If this line throws, it's failed
    }
  )

})


test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_frequency_analysis-report.Rmd", package = "driveanalytics")

  # Markdown parameters
  render_params <- list(
    input_name = "ICD-10 Coded Co-morbidities",
    df_rdv = df_dia,
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
  testthat::expect_snapshot_file(result_html, "report.html")
  # testthat::expect_snapshot_file(result_pdf, "report.pdf") # PDF not supported
  # testthat::expect_snapshot(file.info(result_pdf)$size)

  # browseURL(result_html) # Use this line to run app interactively for debugging
  # browseURL(result_pdf) # Use this line to run app interactively for debugging
})
