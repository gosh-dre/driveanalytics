# TEMPORARY: columns the lookup advertises but the loaded data does not provide
# (icd10_*/drug_* await picture_preprocessing; the rest are unimplemented derivations).
# Hidden from the LLM until the data is fixed. Delete a name here once its column exists.
EXCLUDED_COLUMNS <- c(
  "icd10_chapter", "icd10_section", "icd10_category", "cwt_specific_group",
  "birth_date_age", "age_at_event_years", "admission_type_custom",
  "drug_simple_generic_name", "drug_therapeutic_class_name", "drug_pharmaceutical_class_name"
)

#' Build the analysis-module catalog via R reflection (no file parsing).
#'
#' Discovers shiny modules from the loaded namespaces (a `<mod>_server` paired
#' with a `<mod>_ui`) and reads their parameter names straight off the function
#' signatures with `formals()` — so the catalog always matches the installed code.
#'
#' @param namespaces character; packages to search for `*_server`/`*_ui` functions.
#' @return named list: fn -> list(rpkg = <pkg>, params = <character>).
build_module_catalog <- function(namespaces = c("driveanalytics")) {

  # Modules never offered to the LLM
  exclude <- c("gen_yaml_maker")

  # Resolve a function by name across all namespaces; NULL if it exists in none.
  resolve <- function(fn_name) {
    for (ns in namespaces) {
      fn <- tryCatch(getFromNamespace(fn_name, ns), error = function(e) NULL)
      if (!is.null(fn)) return(fn)
    }
    NULL
  }

  # Formal argument names of a function, or character(0) if it doesn't exist.
  param_names <- function(fn_name) {
    fn <- resolve(fn_name)
    if (is.null(fn)) return(character(0))
    names(formals(fn))
  }

  # Discover candidate modules from every <mod>_server in the namespaces.
  servers <- unlist(lapply(namespaces, function(ns) {
    objs <- tryCatch(ls(getNamespace(ns)), error = function(e) character(0))
    grep("_server$", objs, value = TRUE)
  }))
  mods <- unique(sub("_server$", "", servers))
  mods <- setdiff(mods, exclude)


  catalog <- list()
  for (mod in mods) {
    server_fn <- resolve(paste0(mod, "_server"))
    ui_fn     <- resolve(paste0(mod, "_ui"))
    if (is.null(server_fn) || is.null(ui_fn)) next

    params <- unique(c(names(formals(server_fn)), names(formals(ui_fn))))
    # NOTE: this denylist does not yet drop eval/file-path params — see inst/python/llm_yaml/SECURITY.md
    params <- setdiff(params, c("id", "input", "output", "session"))

    catalog[[mod]] <- list(
      rpkg   = environmentName(environment(server_fn)),
      params = params
    )
  }

  catalog
}

#' Build the RDV dict from picture.platform lookup tables (no parquet/CSV in Python).
#'
#' @return named list: slug -> list(entity, columns = list(list(name, input_type, filter_type, label))).
build_rdv_dict <- function() {
  codes     <- picture.platform:::get_rdv_code_lookup()      # rdv_code, rdv_type, label, ...
  variables <- picture.platform:::get_rdv_Variable_lookup()  # internal -> needs :::

  variables <- dplyr::filter(variables, !(variable_code %in% EXCLUDED_COLUMNS))

  rdv_dict <- list()
  for (i in seq_len(nrow(codes))) {
    code <- codes$rdv_code[i]
    rows <- dplyr::filter(variables, rdv_code == code)

    # One record per column of this RDV (iterate rows, not data-frame columns).
    columns <- lapply(seq_len(nrow(rows)), function(j) {
      list(
        name        = rows$variable_code[j],
        input_type  = rows$input_type[j],
        filter_type = rows$filter_type[j],
        label       = rows$label[j]
      )
    })

    rdv_dict[[code]] <- list(
      entity  = codes$rdv_type[i],
      columns = columns
    )
  }
  rdv_dict
}

#' Generate a PICTURE app YAML from a free-text description (Python Bridge)
#'
#' @param prompt character; free-text description of the desired app; required.
#' @param dataset character; dataset slug passed straight through (premade, not LLM'd).
#' @param creator character; optional creator email (premade).
#' @param img character; optional image path for the app (premade).
#' @param progress function; optional callback `function(stage, fraction)` invoked after each
#'   pipeline stage so a Shiny client can report live progress. NULL (default) disables reporting.
#' @return list; `list(ok = TRUE, yaml = <chr>)` on success, or
#'   `list(ok = FALSE, kind = "refused"|"error", message = <chr>)` when the request is turned
#'   down by the triage gate or generation fails.
#' @export
gen_yaml_maker <- function(prompt, dataset = "dmv", creator = NULL, img = NULL, progress = NULL) {

  # 1. Build ground truth (catalog + RDV dict) from the installed code
  catalog  <- build_module_catalog()
  rdv_dict <- build_rdv_dict()

  logger::log_info("Started gen_yaml_maker (via Reticulate)")

  # 2. Locate Python entry point
  py_root <- system.file("python", package = "driveanalytics")
  py_path <- file.path(py_root, "llm_yaml", "entry.py")

  if (!file.exists(py_path)) {
    logger::log_error(paste("Python entry not found at:", py_path))
    stop("llm_yaml/entry.py not found. Check the installed package.")
  }

  # 3. Import as a package (not source_python) so the module's relative imports
  #    resolve. Python deps come from the ephemeral env declared in zzz.R.
  entry <- reticulate::import_from_path("llm_yaml.entry", path = py_root)

  # Bridge Python pipeline events into the R logger (and thus the app's JSON log file).
  # `message` is interpolated as a value, not a template, so arbitrary LLM text (braces, %) is
  # logged verbatim. Called only from Python's main thread — safe to call back into R here.
  log_event <- function(level, message) {
    msg <- as.character(message)
    switch(
      toupper(as.character(level)),
      ERROR   = logger::log_error("gen_yaml_maker | {msg}"),
      WARN    = logger::log_warn("gen_yaml_maker | {msg}"),
      WARNING = logger::log_warn("gen_yaml_maker | {msg}"),
      logger::log_info("gen_yaml_maker | {msg}")
    )
  }

  # 4. Call Python Function
  out <- tryCatch(
    entry$generate_yaml(
      prompt    = prompt,
      catalog   = catalog,
      rdv_dict  = rdv_dict,
      dataset   = dataset,
      creator   = creator,
      img       = img,
      progress  = progress,
      log_event = log_event
    ),
    error = function(e) {
      logger::log_error(logger::skip_formatter(paste("Python Execution Error:", e$message)))
      list(ok = FALSE, kind = "error", message = e$message)
    }
  )

  if (is.list(out) && isTRUE(out$refused)) {
    reason <- as.character(out$reason)
    logger::log_info("gen_yaml_maker: request refused by triage gate | {reason}")
    return(list(ok = FALSE, kind = "refused", message = out$reason))
  }
  if (is.list(out) && isFALSE(out$ok)) {
    return(out)
  }

  logger::log_info("Finished gen_yaml_maker")
  list(ok = TRUE, yaml = out)
}
