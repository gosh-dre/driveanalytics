test_that("run data", {

  gfa <- driveanalytics::gen_frequency_analysis(df_dia, df_pde, "diag_name", "cohort")
  gca <- driveanalytics::gen_correlation_analysis(gfa, "A")

  gca_head <- head(gca, 10, col = "or_value")
  gca_tbl_raw <- tabulate(gca_head)
  gca_tbl_html <- tabulate(gca_head, "html")
  plt <- plot(gca_head)

  # Types
  testthat::expect_s3_class(gfa, "gen_frequency_analysis")
  testthat::expect_s3_class(gca, "gen_correlation_analysis")
  testthat::expect_s3_class(gca_head, "gen_correlation_analysis")
  testthat::expect_s3_class(gca_tbl_raw, "tbl_df")
  testthat::expect_s3_class(gca_tbl_html, "kableExtra")

  # Plot
  testthat::expect_s3_class(plt, "ggplot")
  # vdiffr::expect_doppelganger("plt-ggplot", plt)

  # Manually compute the values for a given event type
  check_result <- function(x ,unused) {
    cohort_counts <- df_dia %>%
      dplyr::filter(diag_name %in% x[["event"]]) %>%
      dplyr::select(cohort_id, cohort) %>%
      dplyr::distinct() %>%
      dplyr::group_by(cohort) %>%
      dplyr::count()

    cohort_sizes <- df_pde %>%
      dplyr::group_by(cohort) %>%
      dplyr::count(name = "total")

    combi <- cohort_counts %>%
      dplyr::full_join(cohort_sizes, by = "cohort", ) %>%
      dplyr::ungroup() %>%
      tidyr::replace_na(list(n = 0)) %>%
      dplyr::mutate(
        iszero = min(n) == 0,
        n = dplyr::if_else(iszero, n + 0.5, n),
        total = dplyr::if_else(iszero, total + 1, total),
        p = n / total,
        m = total - n,
        od = n / m
      )
    A <- combi[combi$cohort == "A",]
    B <- combi[combi$cohort == "B",]

    # Compute relative risk and odds ratio
    rr <- B$p / A$p
    or <- B$od / A$od

    # Compute standard error and confidence intervals
    rr_se <- sqrt(1/A$n + 1/B$n - 1/A$total - 1/B$total)

    or_se <- sqrt(1/A$n + 1/B$n + 1/A$m + 1/B$m)

    expected <- tibble::tibble(
      event = x[["event"]],
      B.rr_value = rr,
      B.rr_ci_lower = exp(log(rr) - 1.96 * rr_se),
      B.rr_ci_upper = exp(log(rr) + 1.96 * rr_se),
      B.or_value = or,
      B.or_ci_lower = exp(log(or) - 1.96 * or_se),
      B.or_ci_upper = exp(log(or) + 1.96 * or_se)
    )

    # Test values
    testthat::expect_equal(x, expected, tolerance = 1e-6, label = x[["event"]])
  }
  dplyr::sample_n(gca, 30) %>%
    dplyr::arrange(dplyr::desc("B.rr_value")) %>%
    dplyr::rowwise() %>%
    dplyr::group_map(check_result)


})


test_that("shiny", {

  app_ui <- shiny::fluidPage(driveanalytics::gen_correlation_analysis_ui("test"))
  app_server <- function(input, output, session) {
    rdv_in <- shiny::reactive({df_dia})
    pde_in <- shiny::reactive({df_pde})
    driveanalytics::gen_correlation_analysis_server("test", rdv_in, pde_in, "diag_name", "cohort")
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_equal(class(app), "shiny.appobj")

  # shiny::runApp(app) # Use this line to run app interactively for debugging
})


test_that("report generation as HTML", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_correlation_analysis-report.Rmd", package = "driveanalytics")
  output_dir <- withr::local_tempdir(clean = FALSE)

  render_params <- list(
    input_name = "ICD-10 Coded Diagnoses",
    df_rdv = df_dia,
    df_pde = df_pde,
    event_col = "diag_name",
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
