#' Shiny UI Module for Competing Risk Analysis
#'
#' Creates interface for analyzing disease progression with baseline/outcome stage selection.
#'
#' @param id Namespace identifier
#' @param baseline_stages Default baseline stages
#' @param outcome_stage Default outcome stage
#' @param title Optional tab box title
#' @return Shiny tagList
#' @family gen_competing_risk_analysis
#' @export
gen_competing_risk_analysis_ui <- function(id, baseline_stages = c(3, 4), outcome_stage = 5, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(
        title = "Risk Analysis",
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput(ns("comp_risk_outcome"), "Outcome stage", choices = 1:5, selected = outcome_stage)),
          shiny::column(3, shiny::selectInput(ns("comp_risk_baseline"), "Baseline stage", choices = 1:5, selected = baseline_stages, multiple = TRUE)),
          shiny::column(3, shiny::checkboxInput(ns("merge_plots"), "Merge into single plot", value = FALSE))
        ),
        shiny::uiOutput(ns("cif_plots_ui"))
      ),
      ui_info_panel("gen_competing_risk_analysis-text.Rmd")
    )
  )
}


#' Shiny Server Module for Competing Risk Analysis
#'
#' Reactive server logic for Aalen-Johansen competing risk analysis.
#'
#' @param id Namespace identifier
#' @param df_dia Reactive returning diagnosis data
#' @param df_pde Reactive returning patient demographic data
#' @param stratify_by Column for cohort stratification
#' @return Shiny moduleServer
#' @family gen_competing_risk_analysis
#' @export
gen_competing_risk_analysis_server <- function(id, df_dia, df_pde, stratify_by = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Convert date columns to character for Python/reticulate compatibility
    convert_dates <- function(df) {
      if (is.null(df)) return(NULL)
      date_cols <- names(df)[sapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")))]
      for (col in date_cols) df[[col]] <- as.character(df[[col]])
      df
    }
    
    # Run analysis (debounced to avoid rapid re-execution)
    analysis_result <- reactive({
      baseline <- as.numeric(input$comp_risk_baseline)
      outcome <- as.numeric(input$comp_risk_outcome)
      shiny::req(length(baseline) > 0, length(outcome) > 0)
      
      df_dia_converted <- convert_dates(df_dia())
      df_pde_converted <- convert_dates(df_pde())
      
      gen_competing_risk_analysis(df_dia_converted, df_pde_converted, baseline, outcome, stratify_by)
    }) %>% debounce(800)

    # Dynamically create plot containers
    output$cif_plots_ui <- shiny::renderUI({
      result <- analysis_result()
      if (is.null(result) || is.null(result$cif_plot_json)) return(NULL)
      
      ns <- session$ns
      
      if (isTRUE(input$merge_plots)) {
        return(shiny::fluidRow(
          shiny::column(12, plotly::plotlyOutput(ns("plot_merged"), height = "600px"))
        ))
      }
      
      # Individual plots in rows of 2
      plot_names <- names(result$cif_plot_json)
      rows <- split(plot_names, ceiling(seq_along(plot_names) / 2))
      do.call(shiny::tagList, lapply(rows, function(row_names) {
        shiny::fluidRow(
          lapply(row_names, function(name) {
            shiny::column(12 / length(row_names), plotly::plotlyOutput(ns(paste0("plot_", name)), height = "400px"))
          })
        )
      }))
    })
    
    # Render plots
    shiny::observe({
      result <- analysis_result()
      if (is.null(result) || is.null(result$cif_plot_json)) return()
      
      if (isTRUE(input$merge_plots)) {
        # Merged plot: extract CIF lines from all cohorts
        output$plot_merged <- plotly::renderPlotly({
          traces <- lapply(names(result$cif_plot_json), function(name) {
            plot_data <- jsonlite::fromJSON(result$cif_plot_json[[name]], simplifyVector = FALSE)
            cif_trace <- plot_data$data[[3]]  # CIF line is 3rd trace
            cif_trace$name <- name
            cif_trace$showlegend <- TRUE
            cif_trace
          })
          
          fig <- plotly::plot_ly()
          for (trace in traces) {
            fig <- plotly::add_trace(fig, 
                                     x = trace$x, y = trace$y,
                                     type = "scatter", mode = trace$mode,
                                     line = trace$line, name = trace$name,
                                     hovertemplate = trace$hovertemplate)
          }
          
          plotly::layout(fig,
                        title = "Competing Risk Analysis: All Groups",
                        xaxis = list(title = "Days from Baseline"),
                        yaxis = list(title = "Cumulative Incidence", range = c(0, 1)),
                        hovermode = "x unified",
                        plot_bgcolor = "white", paper_bgcolor = "white",
                        showlegend = TRUE)
        })
      } else {
        # Individual plots
        lapply(names(result$cif_plot_json), function(name) {
          local({
            local_name <- name
            local_json <- result$cif_plot_json[[local_name]]
            output[[paste0("plot_", local_name)]] <- plotly::renderPlotly({
              plotly::as_widget(jsonlite::fromJSON(local_json, simplifyVector = FALSE))
            })
          })
        })
      }
    })
  })
}
