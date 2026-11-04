#' Shiny UI module for creating the yamls used to make apps
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_yaml_maker
#' @export
gen_yaml_maker_ui <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        shinyjs::useShinyjs(),
        bs4Dash::tabBox(
            title="AI app maker",
            width=12,
            side="right",

            shiny::tabPanel(
                title="Agent Input",
                shiny::fluidRow(
                    shiny::column(
                        width=7,
                        bs4Dash::box(
                            title="Info",
                            width=12,
                            shiny::textInput(
                                ns("creator_name"),
                                "Creator of the app"
                            ),
                            shiny::uiOutput(ns("image_select")),
                            shiny::uiOutput(ns("dataset_select"))
                        ),
                        bs4Dash::box(
                            title="Text Input",
                            width=12,
                            shiny::p(
                                "Input a description of an analysis or app you would like to make in PICTURE"
                            ),
                            shiny::textAreaInput(
                                ns("user_prompt"),
                                label="Write prompt here",
                                resize="vertical",
                                rows=5,
                                placeholder=paste(
                                    "e.g. Build an app for children admitted with asthma in the last",
                                    "year. Show their age distribution and a survival analysis of time",
                                    "to readmission."
                                )
                            ),
                            shiny::actionButton(
                                ns("start_pipline_button"),
                                "Submit Prompt",
                                icon=shiny::icon("wand-magic-sparkles"),
                                class="btn-primary"
                            )
                        ),
                        bs4Dash::box(
                            title="Help",
                            width=12,
                            collapsible=TRUE,
                            collapsed=TRUE,
                            shiny::p(
                                "Describe the app you want in plain English. Mention:"
                            ),
                            shiny::tags$ul(
                                shiny::tags$li(
                                    shiny::strong("Who"), " — the cohort of patients (e.g. condition, age, dates)."
                                ),
                                shiny::tags$li(
                                    shiny::strong("What"), " — the analyses or charts you want to see."
                                )
                            ),
                            shiny::p(
                                "An AI pipeline turns this into a PICTURE app definition. ",
                                "Choose a dataset and image above, then click ",
                                shiny::strong("Submit Prompt"), ". Generation takes a little while; ",
                                "the progress bar shows the current stage."
                            ),
                            shiny::hr(),
                            shiny::strong("Try an example:"),
                            shiny::tags$ul(
                                lapply(seq_along(.EXAMPLE_PROMPTS), function(i) {
                                    shiny::tags$li(
                                        shiny::actionLink(
                                            ns(paste0("example_", i)),
                                            names(.EXAMPLE_PROMPTS)[i]
                                        )
                                    )
                                })
                            )
                        )
                    ),

                    shiny::column(
                        width=5,
                        style="position: sticky; top: 20px; align-self: flex-start;",
                        bs4Dash::box(
                            title = "Yaml output",
                            width = 12,
                            collapsible = FALSE,
                            shiny::div(
                                style = "height: 80vh; overflow-y: auto; padding-right: 10px;",
                                shiny::uiOutput(ns("yaml_out"))
                            ),
                            shiny::fluidRow(
                                shiny::column(
                                    width = 8,
                                    shiny::textInput(
                                        ns("yaml_out_filename"),
                                        "Filename"
                                    )
                                ),
                                shiny::column(
                                    width = 4,
                                    shiny::actionButton(
                                        ns("yaml_out_button"),
                                        "Create YAML"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}

#' Shiny server module for creating the yamls used to make apps
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param datasets_list character; optional vector of dataset names to offer; derived from `datasets_path` when NULL.
#' @param datasets_path character; path searched for available datasets when `datasets_list` is NULL.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_yaml_maker
#' @export
gen_yaml_maker_server <- function(id, datasets_list = NULL, datasets_path = "picture.platform/DummyData") {

    shiny::moduleServer(
        id,
        function(input, output, session) {

            datasets <- if (!is.null(datasets_list)) {
                datasets_list
            } else {
                .datasets(datasets_path)
            }

            output$dataset_select <- shiny::renderUI({
                shiny::selectInput(
                    session$ns("dataset_select"),
                    label = "Select dataset using",
                    choices = datasets
                )
            })
            output$image_select <- shiny::renderUI({
                shiny::selectInput(
                    session$ns("image_select"),
                    label = "Choose image for the app",
                    choices = .get_images()
                )
            })

            # Populate the prompt box when an example link is clicked.
            lapply(seq_along(.EXAMPLE_PROMPTS), function(i) {
                shiny::observeEvent(input[[paste0("example_", i)]], {
                    shiny::updateTextAreaInput(
                        session, "user_prompt", value = .EXAMPLE_PROMPTS[[i]]
                    )
                })
            })

            yaml_text <- shiny::reactiveVal("")

            shiny::observeEvent(input$start_pipline_button, {
                prompt <- input$user_prompt
                if (is.null(prompt) || !nzchar(trimws(prompt))) {
                    shiny::showNotification(
                        "Please enter a prompt describing the app you want before submitting.",
                        type = "warning"
                    )
                    return(invisible(NULL))
                }
                if (nchar(prompt) > .MAX_PROMPT_CHARS) {
                    shiny::showNotification(
                        sprintf("Prompt is too long (%d characters; limit is %d). Please shorten it.",
                                nchar(prompt), .MAX_PROMPT_CHARS),
                        type = "warning"
                    )
                    return(invisible(NULL))
                }

                shinyjs::disable("start_pipline_button")
                on.exit(shinyjs::enable("start_pipline_button"))

                prog <- shiny::Progress$new()
                on.exit(prog$close(), add = TRUE)
                prog$set(message = "Generating app", value = 0)

                # stage key -> user-facing detail; the Python pipeline reports the keys.
                stage_text <- c(
                    triage          = "Checking your request…",
                    agents          = "Querying cohort & analysis agents…",
                    cohort          = "Cohort agent returned…",
                    analysis        = "Analysis agent returned…",
                    validate        = "Validating results…",
                    refine          = "Re-running cohort & analysis agents…",
                    refine_cohort   = "Re-running cohort agent…",
                    refine_analysis = "Re-running analysis agent…",
                    assemble        = "Assembling app…",
                    finalise        = "Finalising YAML…"
                )
                report <- function(stage, fraction) {
                    detail <- unname(stage_text[stage])
                    prog$set(value = fraction, detail = if (is.na(detail)) stage else detail)
                }

                out <- gen_yaml_maker(
                    input$user_prompt,
                    input$dataset_select,
                    input$creator_name,
                    input$image_select,
                    progress = report
                )
                if (!isTRUE(out$ok)) {
                    msg <- if (!is.null(out$message)) out$message else
                        "YAML generation failed. Please try again; see the R console/logs for details."
                    shiny::showNotification(
                        msg,
                        type = if (identical(out$kind, "refused")) "warning" else "error"
                    )
                    yaml_text("")
                    return(invisible(NULL))
                }
                yaml_text(out$yaml)
            })

            output$yaml_out <- shiny::renderUI({
                yaml <- yaml_text()
                if (!nzchar(yaml)) {
                    return(
                        shiny::div(
                            style = "color: #888; font-style: italic; padding: 1em;",
                            "Your generated app YAML will appear here once you submit a prompt."
                        )
                    )
                }
                shiny::tags$pre(yaml)
            })

            shiny::observeEvent(input$yaml_out_button, {
                if (!nzchar(yaml_text())) {
                    shiny::showNotification(
                        "There is no generated YAML to save yet. Submit a prompt first.",
                        type = "warning"
                    )
                    return(invisible(NULL))
                }
                .create_yaml(input, yaml_text())
            })
        }
    )
}

.MAX_PROMPT_CHARS <- 4000L

# Example prompts offered in the Help box; names are the clickable labels, values fill the input.
.EXAMPLE_PROMPTS <- c(
    "Asthma readmissions" = paste(
        "Build an app for children admitted with asthma in the last year.",
        "Show their age distribution and a survival analysis of time to readmission."
    ),
    "Diabetes overview" = paste(
        "Create a dashboard for patients diagnosed with type 1 diabetes,",
        "summarising counts by sex and a trend of admissions over time."
    ),
    "Surgery length of stay" = paste(
        "Make an app for patients who had cardiac surgery, comparing",
        "length of stay across age groups."
    )
)

# Current implementation will need to be changed depending on final structure of different datasets
.datasets <- function(datasets_path) {
    if (!dir.exists(datasets_path)) {
        return(NULL)
    }
    dirs <- list.dirs(datasets_path, recursive=FALSE)
    datasets <- vapply(dirs, function(dir) {
        files <- list.files(dir)
        if (length(files) == 0){
            return("___NA___")
        }
        file <- files[[1]]
        match_position <- regexpr("^[^_]+_", file)
        dataset <- regmatches(file, match_position)
        sub("_", "", dataset)
    }, character(1))

    datasets <- unique(datasets)
    datasets[! datasets %in% "___NA___"]
}

.create_yaml <- function(input, yaml_out, path = "picture.platform/inst/apps", session = shiny::getDefaultReactiveDomain()) {
    raw_name  <- if (is.null(input$yaml_out_filename)) "" else input$yaml_out_filename
    safe_name <- sub("\\.yaml$", "", basename(trimws(raw_name)), ignore.case = TRUE)

    if (!nzchar(safe_name) || !grepl("^[A-Za-z0-9_-]+$", safe_name)) {
        shiny::showNotification(
            "Invalid filename. Use only letters, numbers, hyphens and underscores (no spaces, dots or slashes).",
            type = "error", session = session
        )
        return(invisible(NULL))
    }

    file_path <- file.path(path, paste0(safe_name, ".yaml"))
    dir_path  <- dirname(file_path)

    if (file.exists(file_path)) {
        shiny::showNotification(sprintf("File '%s' already exists. Not overwriting.", file_path), type = "error", session = session)
        return (invisible(NULL))
    } else if (!dir.exists(dir_path)) {
        shiny::showNotification(sprintf("Directory '%s' does not exist. Please check the filename or path.", dir_path), type = "error", session = session)
        return (invisible(NULL))
    } else {
        write(yaml_out, file = file_path)
        shiny::showNotification(sprintf("YAML file written to '%s'", file_path), type = "message", session = session)
        return (invisible(NULL))
    }
}

.get_images <- function() {
    image_paths <- list.files("picture.platform/inst/www/images", full.names = TRUE)

    image_names <- list.files("picture.platform/inst/www/images", full.names = FALSE)
    image_names <- gsub("\\.(png|jpg)$", "", image_names)
    names(image_paths) <- image_names
    return(image_paths)
}