test_that("basic", {

  df_char <- pde_cohort_characteristics(df_pde)
  tbl_html <- tabulate(df_char, format = "html")
  tbl_tex <- tabulate(df_char, format = "latex")

  # Types
  testthat::expect_s3_class(df_char, "pde_cohort_characteristics")
  testthat::expect_s3_class(tbl_html, "kableExtra")
  testthat::expect_s3_class(tbl_tex, "knitr_kable")

  # Prep data for comparison
  df_out <- as.data.frame(df_char)
  row.names(df_out) <- paste(df_out$grouping, df_out$Characteristic, sep = "::")

  # Check values
  test_cohort <- function(coh) {
    df <- dplyr::filter(df_pde, cohort == coh)
    n <- nrow(df)
    strip_pct <- function(x) as.integer(stringr::str_remove(x, " %"))
    strip_yrs <- function(x) as.integer(stringr::str_remove(x, " years"))
    calc_pct <- function(x) x / n * 100
    testthat::expect_equal(as.integer(df_out["N::Periods",coh]), n)
    testthat::expect_equal(as.integer(df_out["N::Patients",coh]), length(unique(df$project_id)))
    testthat::expect_equal(strip_pct(df_out["Sex::Female",coh]), calc_pct(sum(df$sex_name == "Female")))
    testthat::expect_equal(strip_pct(df_out["Sex::Male",coh]), calc_pct(sum(df$sex_name == "Male")))
    testthat::expect_true(all(tail(df_out, n = 5)[,3:4] == "0.00 years"))
  }
  test_cohort("A")
  test_cohort("B")

})


test_that("shiny", {

  app_ui <- shiny::fluidPage(driveanalytics::pde_cohort_characteristics_ui("test"))
  app_server <- function(input, output, session) {
    rdv_in <- shiny::reactive({df_pde})
    driveanalytics::pde_cohort_characteristics_server("test", rdv_in)
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_equal(class(app), "shiny.appobj")

  # shiny::runApp(app) # Use this line to run app interactively for debugging

})


test_that("markdown", {
  rmd_file <- system.file("rmd", "pde_cohort_characteristics-report.Rmd", package = "driveanalytics")
  testthat::skip(message = "Not working on Gitlab CI")
  # Markdown parameters
  render_params <- list(
    df_pde = df_pde
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
