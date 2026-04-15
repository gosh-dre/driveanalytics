# Allows the the python modules to run
.onLoad <- function(libname, pkgname) {
  # Use py_require() which works with existing ephemeral environments
  reticulate::py_require(
    c("pandas", "numpy", "lifelines", "plotly"),
    action = "add"
  )
}
