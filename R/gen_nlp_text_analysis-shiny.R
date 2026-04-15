#' Shiny UI Module for NLP Text Analysis
#'
#' Creates tabbed interface for NRC emotion analysis with prevalence, evolution, and top words.
#'
#' @param id Namespace identifier
#' @param title Optional title for tab box
#' @return Shiny tagList
#' @family gen_nlp_text_analysis
#' @export
gen_nlp_text_analysis_ui <- function(id, title = NULL) {
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      
      shiny::tabPanel(
        title = "Emotion Prevalence",
        plotly::plotlyOutput(ns("topic_prevalence_plot"), height = "600px"),
        shiny::br(),
        shiny::p(
          shiny::tags$strong("NRC Emotions:"), " Detects 8 emotions: anger, fear, sadness, joy, trust, disgust, surprise, anticipation",
          shiny::tags$br(),
          shiny::tags$strong("Negation:"), " Handles phrases like 'not happy' (→ sadness), 'no fear' (→ trust)",
          shiny::tags$br(),
          shiny::tags$em("NRC Emotion Lexicon by Saif Mohammad and Peter Turney")
        )
      ),
      
      shiny::tabPanel(
        title = "Emotion Evolution",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::sliderInput(
              ns("resolution"),
              "Time Resolution",
              min = 10,
              max = 100,
              value = 50,
              step = 5
            )
          ),
          shiny::column(
            4,
            shiny::selectInput(
              ns("time_mode"),
              "Time Mode",
              choices = c(
                "Normalized (0-100%)" = "normalized",
                "Relative (Days)" = "relative",
                "Absolute (Dates)" = "absolute"
              ),
              selected = "normalized"
            )
          )
        ),
        shiny::uiOutput(ns("evolution_plots_ui")),
        shiny::br(),
        shiny::p(
          shiny::tags$strong("Normalized:"), " Each patient's timeline scaled to 0-100%",
          shiny::tags$br(),
          shiny::tags$strong("Relative:"), " Days since first response",
          shiny::tags$br(),
          shiny::tags$strong("Absolute:"), " Calendar dates"
        )
      ),
      
      shiny::tabPanel(
        title = "Top Words",
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(
              ns("n_words"),
              "Words per Emotion",
              value = 10,
              min = 5,
              max = 50,
              step = 5
            )
          )
        ),
        DT::dataTableOutput(ns("top_words_table")),
        shiny::br(),
        shiny::p("Most frequent words (including negated phrases like 'not happy') for each emotion.")
      ),
      
      picture.platform::ui_info_panel(
        system.file("about", "gen_nlp_text_analysis-text.Rmd", package = "driveanalytics")
      )
    )
  )
}


#' Shiny Server Module for NLP Text Analysis
#'
#' Reactive server logic for NRC emotion analysis.
#'
#' @param id Namespace identifier
#' @param df_rdv_server Reactive returning filtered RDV data
#' @param text_col Column name containing text
#' @param stratify_by Column for cohort stratification (defaults to "cohort")
#' @return Shiny moduleServer
#' @family gen_nlp_text_analysis
#' @export
gen_nlp_text_analysis_server <- function(id, df_rdv_server, text_col = NULL, stratify_by = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Run emotion analysis model
      model_result <- shiny::reactive({
        df <- df_rdv_server()
        if (is.null(df) || nrow(df) == 0) return(NULL)
        
        driveanalytics::gen_nlp_text_analysis_model(
          df_rdv = df,
          text_col = text_col,
          stratify_by = stratify_by
        )
      })
      
      # Render prevalence plot
      output$topic_prevalence_plot <- plotly::renderPlotly({
        result <- model_result()
        if (is.null(result)) return(NULL)
        driveanalytics::gen_nlp_topic_prevalence(result)
      })
      
      # Dynamically create evolution plot containers
      output$evolution_plots_ui <- shiny::renderUI({
        result <- model_result()
        if (is.null(result)) return(shiny::p("No data available."))
        
        if (!all(c("start_datetime", "project_id") %in% names(result$doc_metadata))) {
          return(shiny::p("Evolution requires 'start_datetime' and 'project_id' columns."))
        }
        
        ns <- session$ns
        cohort_levels <- levels(result$doc_metadata[[result$stratify_by]])
        
        plot_outputs <- lapply(cohort_levels, function(cohort_name) {
          shiny::div(
            shiny::h4(paste0("Cohort: ", cohort_name)),
            plotly::plotlyOutput(ns(paste0("evolution_", cohort_name)), height = "500px"),
            shiny::br()
          )
        })
        
        do.call(shiny::tagList, plot_outputs)
      })
      
      # Render evolution plots for each cohort
      shiny::observe({
        result <- model_result()
        if (is.null(result) || is.null(result$doc_metadata)) return()
        
        evolution_plots <- driveanalytics::gen_nlp_topic_evolution(
          result, 
          resolution = input$resolution, 
          time_mode = input$time_mode
        )
        
        if (!is.null(evolution_plots) && length(evolution_plots) > 0) {
          lapply(names(evolution_plots), function(cohort_name) {
            local({
              local_cohort <- cohort_name
              output_name <- paste0("evolution_", local_cohort)
              output[[output_name]] <- plotly::renderPlotly({
                evolution_plots[[local_cohort]]
              })
            })
          })
        }
      })
      
      # Render top words table
      output$top_words_table <- DT::renderDT({
        result <- model_result()
        if (is.null(result) || is.null(result$emotion_data)) return(NULL)
        driveanalytics::gen_nlp_top_words(result, n_words = input$n_words)
      })

    }
  )
}