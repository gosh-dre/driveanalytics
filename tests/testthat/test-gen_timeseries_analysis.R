test_that("run", {

  gta <- gen_timeseries_analysis(
    df_flw_main, df_pde, "type", "value", "taken_datetime", "cohort",
    hor_axis = "Years since birth", overlay_cohorts = TRUE
  )

  testthat::expect_s3_class(gta$df, "tbl_df")
  testthat::expect_s3_class(gta$plt, "ggplot")
})


test_that("shiny app", {

  app_ui <- shiny::fluidPage(

    shiny::includeCSS(system.file("www", "style.css", package = "picture.platform")),
    driveanalytics::gen_timeseries_analysis_ui("test")
  )

  df_flw_filter <- df_flw_main %>%
    dplyr::filter(cohort == "B" | project_id == "PR-000037")

  app_server_flw <- function(input, output, session) {
    flw_in <- shiny::reactive({df_flw_filter})
    pde_in <- shiny::reactive({df_pde})
    driveanalytics::gen_timeseries_analysis_server(
      "test", flw_in, pde_in, "value", "taken_datetime", "cohort", "type"
    )
  }

  app_flw <- shiny::shinyApp(ui = app_ui, server = app_server_flw)
  testthat::expect_s3_class(app_flw, "shiny.appobj")

  app_server_lab <- function(input, output, session) {
    lab_in <- shiny::reactive({df_lab_main})
    pde_in <- shiny::reactive({df_pde})
    driveanalytics::gen_timeseries_analysis_server(
      "test", lab_in, pde_in, "Value",
      "collected_datetime", "cohort", "component_name"
    )
  }

  app_lab <- shiny::shinyApp(ui = app_ui, server = app_server_lab)
  testthat::expect_s3_class(app_lab, "shiny.appobj")

  # shiny::runApp(app_flw) # Use this line to run app interactively for debugging
  # shiny::runApp(app_lab) # Use this line to run app interactively for debugging
})


#' Tests that the module has a functional markdown file
test_that("markdown", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_timeseries_analysis-report.Rmd", package = "driveanalytics")

  # Most common component_name
  cn <- names(sort(table(df_lab_main$component_name), decreasing = TRUE))[[1]]
  df_rdv <- df_lab_main %>%
    dplyr::filter(component_name == cn)

  # Markdown parameters
  render_params <- list(
    df_rdv = df_rdv,
    df_pde = df_pde,
    grouping_cols = "component_name",
    value_col = "Value",
    datetime_col = "collected_datetime",
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

  # Tests
  testthat::expect_true(file.exists(result_html))
  testthat::expect_true(file.exists(result_pdf))

  # browseURL(result_html) # Use this line to run app interactively for debugging
  # browseURL(result_pdf) # Use this line to run app interactively for debugging
})
