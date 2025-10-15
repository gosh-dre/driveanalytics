# List files in about dir
about_dir <- "./inst/about" # system.file("about", package = "driveanalytics")
about_files <- list.files(about_dir, pattern = "\\.Rmd$", full.names = TRUE)

# Exclusions (perhaps we just delete these old files)
# about_files <- about_files[!stringr::str_ends(about_files, "gen_location_analysis-text.Rmd")] # bugged
about_files <- about_files[!stringr::str_ends(about_files, "rsk_diabetes-text.Rmd")]
about_files <- about_files[!stringr::str_ends(about_files, "scd_risk_score-text.Rmd")]

# Build rmd_params list
pp_params <- lapply(about_files, function(x) list("rmd_file" = x))

# Prepress
pp_result <- prepressr::prepress(
  pp_params
)

# Copy Eleni's images
file.copy(file.path(about_dir, "img"), pp_result$output_dir, recursive=TRUE)

# Bookdown
bd_file <- bookdown::render_book(
  pp_result$output_dir,
  params = pp_result$params,
  # # To use the PDF, you need to ad the following output format and manually remove all the http svgs from the about files
  # # you can remove the http svgs by find/replacing the following regex: ^!\[\](https.*$
  # output_format = rmarkdown::pdf_document(includes = list(
  #   in_header = system.file("rmd", "templates", "preamble.tex", package = "picture.platform")
  # ))
)

utils::browseURL(bd_file)
