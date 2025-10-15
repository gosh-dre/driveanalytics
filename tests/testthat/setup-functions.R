
# Function to help testing RMD reports
test_report <- function(rmd_file, render_params, skip_html = FALSE) {

  output_dir <- withr::local_tempdir(clean = FALSE)

  # PDF report
  result_pdf <- rmarkdown::render(
    input = rmd_file,
    output_file = output_dir,
    params = render_params,
    output_format = rmarkdown::pdf_document(includes = list(in_header = tex_preamble))
  )

  testthat::expect_true(file.exists(result_pdf))
  # testthat::expect_snapshot_file(result_pdf, "report.pdf") # PDF not supported
  # testthat::expect_snapshot(file.info(result_pdf)$size) # inconsistent

  if (!skip_html) {

    # HTML report
    result_html <- rmarkdown::render(
      input = rmd_file,
      output_file = output_dir,
      params = render_params,
      output_format = rmarkdown::html_document()
    )

    testthat::expect_true(file.exists(result_html))
    testthat::expect_snapshot_file(result_html, "report.html")

  } else {
    result_html <- NULL
  }

  list("html" = result_html, "pdf" = result_pdf)
}
