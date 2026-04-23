#' Shiny UI module for Sentiment vs Severity Analysis
#'
#' @param id Character label of the namespace for the module instance.
#' @param title Optional title for the tab box.
#'
#' @export
gen_sentiment_severity_analysis_ui <- function(id, title = "Sentiment vs Severity Analysis") {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      
      # Tab 1: Visualizations
      shiny::tabPanel(
        title = "Dashboard",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(
              ns("stratify_col"),
              "Stratify Analysis By",
              choices = c("Anatomy" = "anatomy", "None" = "none"), 
              selected = "anatomy"
            )
          ),
          shiny::column(
            4,
            shiny::selectInput(
              ns("severity_filter"),
              "Filter Severity Levels",
              choices = c("All", "Low", "Moderate", "High"),
              selected = "All"
            )
          )
        ),
        shiny::br(),
        shiny::fluidRow(
          # Top row: Severity Distribution and Sentiment Boxplot
          shiny::column(6, plotly::plotlyOutput(ns("severity_bar_plot"))),
          shiny::column(6, plotly::plotlyOutput(ns("sentiment_boxplot")))
        ),
        shiny::br(),
        shiny::fluidRow(
          # Bottom row: Density plot
          shiny::column(12, plotly::plotlyOutput(ns("sentiment_density"), height = "300px"))
        )
      ),
      
      # Tab 2: Data Table
      shiny::tabPanel(
        title = "Summary Table", 
        DT::dataTableOutput(ns("summary_table"))
      ),
      
      # Tab 3: Raw Data View (Optional)
      shiny::tabPanel(
        title = "Processed Data",
        DT::dataTableOutput(ns("raw_table"))
      )
    )
  )
}

#' Shiny server module for Sentiment vs Severity Analysis
#'
#' @param id character label of the namespace.
#' @param df_nlp_server reactive function providing the clinical text dataframe.
#' @param text_col character name of the text column.
#'
#' @export
gen_sentiment_severity_analysis_server <- function(id, 
                                                   df_nlp_server, 
                                                   text_col = "raw_clinical_note") {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
      # Reactive: Filter Data based on UI inputs before analysis
      df_reactive <- shiny::reactive({
        req(df_nlp_server())
        data <- df_nlp_server()
        
        # Simple extraction for filtering purposes (duplicate logic but useful for pre-filter)
        # Ideally, this matches the main analysis logic or we analyze first then filter.
        # Here we pass the raw data to the analysis function which handles logic.
        return(data)
      })
      
      # Compute Analysis
      # We re-run analysis when stratification changes
      analysis_results <- shiny::reactive({
        req(df_reactive())
        
        strat_col <- if(input$stratify_col == "none") NULL else input$stratify_col
        
        gen_sentiment_severity_analysis(
          df_nlp = df_reactive(),
          text_col = text_col,
          stratify_by = strat_col
        )
      })
      
      # Filter plots based on Severity Selection (Post-processing for display)
      # Note: We filter the data INSIDE the plot render or create a filtered object
      
      output$severity_bar_plot <- plotly::renderPlotly({
        req(analysis_results())
        analysis_results()$severity_bar_plot
      })
      
      output$sentiment_boxplot <- plotly::renderPlotly({
        req(analysis_results())
        analysis_results()$sentiment_boxplot
      })
      
      output$sentiment_density <- plotly::renderPlotly({
        req(analysis_results())
        analysis_results()$sentiment_density
      })
      
      output$summary_table <- DT::renderDataTable({
        req(analysis_results())
        DT::datatable(analysis_results()$df, 
                      options = list(scrollX = TRUE))
      })
      
      output$raw_table <- DT::renderDataTable({
        req(analysis_results())
        DT::datatable(analysis_results()$raw_data %>% 
                        dplyr::select(project_id, dplyr::any_of(c("anatomy", "severity_derived", "sentiment_score"))),
                      options = list(pageLength = 5))
      })
      
    }
  )
}