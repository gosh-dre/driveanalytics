#' Shiny UI module for the Assess the correlation of a set of events by cohort
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_correlation_analysis
#' @export
gen_correlation_analysis_ui <- function(id, title = NULL) {

  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",

      shiny::tabPanel(title = "Plot",
                      shiny::plotOutput(ns("corr_plot")),  #, height = "600px"),

                      shiny::fluidRow(

                        shiny::column(3, shiny::selectInput(ns("corr_relative_to"), "Relative to cohort", c())),
                        shiny::column(3, shiny::selectInput(ns("corr_odds_risk"), "Metric", c("Relative risk" = "rr_value", "Odds ratio" = "or_value"))),
                        shiny::column(3, shiny::sliderInput(ns("corr_nplot"), "Number to plot",
                                                            min = 10, max = 60, value = 20)),
                        shiny::column(3, shiny::selectInput(ns("corr_order_by"), "Order by cohort", c()))
                      )
      ),
      shiny::tabPanel(title = "Data", DT::dataTableOutput(ns("corr_table"))),
      picture.platform::ui_info_panel(
        system.file("about", "gen_correlation_analysis-text.html", package = "driveanalytics")
      )
    )

  )

}


#' Shiny server module for the Assess the correlation of a set of events by cohort
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param df_rdv DESCRIPTION.
#' @param event_col DESCRIPTION.
#' @param stratify_by DESCRIPTION.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_correlation_analysis
#' @export
gen_correlation_analysis_server <- function(id, df_rdv, df_pde, event_col, stratify_by) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Reactive on the correlation analysis
      gca <- shiny::reactive({
        gfa <- gen_frequency_analysis(df_rdv(), df_pde(), event_col, stratify_by)
        gen_correlation_analysis(gfa, input$corr_relative_to)
      })

      # Reactive on the possible stratify_by (cohort) values
      strat_levels <- shiny::reactive({
        sl <- levels(as.factor(df_rdv()[[stratify_by]]))

        # Update the relative to selection
        shiny::updateSelectInput(
          session, "corr_relative_to", choices = sl, selected = sl[1]
        )

        return(sl)
      })

      # Update available number to plot UI element
      shiny::observe({

        max_vals <- nrow(gca())
        shiny::updateSliderInput(session, "corr_nplot",
                                 max = min(nrow(gca()), 60))
      })

      # Update 'order by' selection box
      shiny::observeEvent(input$corr_relative_to, {

        # Get the current relative-to selection
        cohorts <- strat_levels()
        rel_to <- input$corr_relative_to

        rel_to <- ifelse(rel_to == "", cohorts[1], rel_to)
        cohort_choices <- cohorts[cohorts != rel_to]

        shiny::updateSelectInput(session, "corr_order_by",
                                 choices = cohort_choices,
                                 selected = cohort_choices[1])

      })

      # Update plot
      output$corr_plot <- shiny::renderPlot({
        corr_dat <- gca()
        if(is.null(corr_dat) || input$corr_order_by == "" || input$corr_order_by == input$corr_relative_to) return()
        gca_top <- head(corr_dat, input$corr_nplot, input$corr_odds_risk, input$corr_order_by)
        plot(gca_top)
      })

      # Update data table
      output$corr_table <- DT::renderDT({
        gca_selected <- select_metric(gca(), input$corr_odds_risk)
        tabulate(gca_selected, "DT")
      })

    }
  )
}
