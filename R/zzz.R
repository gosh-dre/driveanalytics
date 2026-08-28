.onLoad <- function(libname, pkgname) {
  required <- c(
    "pandas", "numpy", "lifelines", "plotly",
    "langgraph>=0.2", "langchain-openai>=0.2", "langchain-core>=0.3",
    "pydantic>=2", "PyYAML>=6.0", "python-dotenv>=1.0"
  )
  try(reticulate::py_require(required, action = "add"), silent = TRUE)
}
