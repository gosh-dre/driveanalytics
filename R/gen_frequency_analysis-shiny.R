#' Shiny UI module for the generic frequncy analysis
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id character; Module namespace ID
#' @param title character; Title to display above the tabbox
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_frequency_analysis
#' @export
gen_frequency_analysis_ui <- function(id, title = NULL) {

  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(title = "Plot",
                      shiny::plotOutput(ns("plot")),
                      shiny::fluidRow(
                        shiny::column(4, shiny::sliderInput(ns("nplot"),
                                                            "Number to plot",
                                                            min = 10,
                                                            max = 30,
                                                            value = 15)),
                        shiny::column(4, shiny::selectInput(ns("order_by"),
                                                            "Order by",
                                                            c())),
                        shiny::column(4, shiny::uiOutput(ns("category")))
                      )
      ),
      shiny::tabPanel(title = "Data", DT::dataTableOutput(ns("table"))),
      picture.platform::ui_info_panel(
        system.file("about", "gen_frequency_analysis-text.html", package = "driveanalytics")
      )
    )
  )
}


#' Shiny server module for the generic frequency analysis
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id character; Module namespace ID.
#' @param df_rdv_server reactive; Input reactive providing input RDV to analyse
#' @param event_col character vector; Column label(s) to calculate frequencies
#' from.
#' @param cohort_col character; Column to define cohorts for the analysis.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_frequency_analysis
#' @export
gen_frequency_analysis_server <- function(id, df_rdv_server, df_pde_server, event_col = NULL,
                                          cohort_col = "cohort", new_col = FALSE, new_col_breaks = NULL, new_col_labels = NULL) {

  # Ensure event_col is a vector not a list
  event_col <- unlist(event_col)

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- shiny::NS(id)

      output$category <- shiny::renderUI({

        if (!is.null(event_col)) {
          event_col <- event_col[sapply(df_rdv_server()[,event_col], function(x)!all(is.na(x)))]
          shiny::selectInput(ns("category"),
                             label = "Frequency based on",
                             choices = event_col,
                             selected = event_col[1])
        } else {
          shiny::selectInput(ns("category"),
                             label = "Frequency based on",
                             choices = "All")
        }

      })

      gfa <- shiny::reactive({
        req(input$category)
        gen_frequency_analysis(df_rdv_server(), df_pde_server(), input$category, cohort_col, new_col, new_col_breaks, new_col_labels)
      })

      shiny::observe({
        shiny::updateSliderInput(session, "nplot", max = min(nrow(gfa()), 30))

        if (!is.null(cohort_col)) {
          strat_levels <- levels(as.factor(df_rdv_server()[[cohort_col]]))
        } else {
          strat_levels <- "NA"
        }

        shiny::updateSelectInput(session, "order_by",
                                 choices = strat_levels,
                                 selected = strat_levels[1])

      })

      output$plot <- shiny::renderPlot({

        gfa_top <- head(gfa(), input$nplot, input$order_by)
        plot(gfa_top)

      })

      output$table <- DT::renderDT(tabulate(gfa(), "DT"))

    }
  )
}
