#' Shiny UI module for the Visualise a selected event
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_event_time_analysis
#' @export
gen_event_time_analysis_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bs4Dash::tabBox(
      title = title,
      side = "right",
      width = 12,
      shiny::tabPanel(title = "Plot",
                      shiny::fluidRow(
                        shiny::column(
                          4,
                          shiny::selectInput(
                            ns("plottype"),
                            "Select plot type",
                            choices = c("Boxplot", "Histogram"),
                            selected = "Boxplot")
                          ),
                        shiny::column(
                          7,
                          offset = 1,
                          shiny::checkboxInput(
                            ns("plotoptions"),
                            label = "Outliers",
                            value = FALSE
                          )
                        ),
                      ),
                      shiny::plotOutput(ns("plot"), height = 1000)
      ),
      shiny::tabPanel(
        #style = "overflow-x:scroll",
        title = "Table",
        shiny::div(DT::dataTableOutput(ns("table")), style = "font-size:80%")),
        picture.platform::ui_info_panel(
          system.file("about", "gen_event_time_analysis-text.html", package = "driveanalytics")
        )
    )
  )
}

#' Shiny server module for the Visualise a selected event
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param event_col character; the column that indicates the event of
#' interest; required.
#' @param stratify_by list of character labels; the column(s) to group the
#' analysis by; optional.
#' @param df_server Reactive server function providing a dataframe
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_event_time_analysis
#' @export
gen_event_time_analysis_server <- function(id, event_col, stratify_by = "cohort",
                                           df_server) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- shiny::NS(id)
      gea <- shiny::reactive({
        shiny::req(input$plottype)
        gen_event_time_analysis(df_server(), event_col, stratify_by,
                                input$plottype, input$plotoptions)
      })

      shiny::observeEvent(input$plottype, {

        if (input$plottype == "Boxplot") {
          shiny::updateCheckboxInput(
            session = session,
            inputId = "plotoptions",
            label = "Show outliers",
            value = FALSE
          )
        } else if (input$plottype == "Histogram") {
          shiny::updateCheckboxInput(
            session = session,
            inputId = "plotoptions",
            label = "Log transform",
            value = FALSE
          )
        }

      })

      output$plot <- shiny::renderPlot({
        gea()$plt
      })

      output$table <- DT::renderDataTable({
        DT::datatable(gea()$df, escape=FALSE, options = list(scrollX = TRUE))
      })

    }
  )
}
