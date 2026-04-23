#' Shiny UI module for Risk Assessment Analysis
#' @param id Character label of the namespace.
#' @param title Optional title for the tab box.
#' @export
gen_risk_assessment_ui <- function(id, title = "CKD Risk Assessment") {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      
      # Tab 1: Visualizations
      shiny::tabPanel(
        title = "Risk Graphs",
        shiny::fluidRow(
          shiny::column(12, 
            plotly::plotlyOutput(ns("risk_dist"), height = "400px", width="50%")
          )
        ),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(6, plotly::plotlyOutput(ns("egfr_trop"), height = "500px")),
          shiny::column(6, plotly::plotlyOutput(ns("egfr_bnp"), height = "500px"))
        )
      ),
      
      # Tab 2: Summary Table
      shiny::tabPanel(
        title = "Summary Stats", 
        DT::dataTableOutput(ns("summary_table"))
      ),
      
      # Tab 3: Processed Data
      shiny::tabPanel(
        title = "Patient Risk Data",
        DT::dataTableOutput(ns("raw_table"))
      )
    )
  )
}

#' Shiny server module for Risk Assessment Analysis
#' @param id character label of the namespace.
#' @param df_ckd reactive function providing the input data.
#' @export
gen_risk_assessment_server <- function(id, df_ckd) { 
  shiny::moduleServer(id, function(input, output, session) {
    
    # Process data through logic
    analysis_results <- shiny::reactive({
      shiny::req(df_ckd()) 
      
      # Pass it to the logic function
      gen_risk_assessment(df_ckd = df_ckd())
    })
      
    # --- Render Plotly Plots from JSON ---
    
    output$risk_dist <- plotly::renderPlotly({
      shiny::req(analysis_results()$plot_json$risk_distribution)
      plotly::as_widget(jsonlite::fromJSON(analysis_results()$plot_json$risk_distribution, 
                                           simplifyVector = FALSE))
    })
      
    output$egfr_trop <- plotly::renderPlotly({
      shiny::req(analysis_results()$plot_json$egfr_troponin)
      plotly::as_widget(jsonlite::fromJSON(analysis_results()$plot_json$egfr_troponin, 
                                           simplifyVector = FALSE))
    })

    output$egfr_bnp <- plotly::renderPlotly({
      shiny::req(analysis_results()$plot_json$egfr_bnp)
      plotly::as_widget(jsonlite::fromJSON(analysis_results()$plot_json$egfr_bnp, 
                                           simplifyVector = FALSE))
    })
      
    # --- Render Tables ---
    
    output$summary_table <- DT::renderDataTable({
      shiny::req(analysis_results()$df)
      DT::datatable(analysis_results()$df, options = list(scrollX = TRUE))
    })
      
    output$raw_table <- DT::renderDataTable({
      shiny::req(analysis_results()$raw_data)
      DT::datatable(analysis_results()$raw_data, options = list(pageLength = 10))
    })
  })
}