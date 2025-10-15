#' Shiny UI module for the Event count analysis
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_event_count
#' @export
gen_event_count_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(
        title = "Plot",
        shiny::plotOutput(ns("plot")) #,
        # shiny::fluidRow(
        #   shiny::column(4, shiny::sliderInput(ns("nplot"), "Number to plot", min = 10, max = 60, value = 30)),
        #   shiny::column(4, shiny::selectInput(ns("order_by"), "Order by", c())),
        #   shiny::column(4, shiny::uiOutput(ns("category")))
        # )
      ),
      shiny::tabPanel(title = "Data", DT::dataTableOutput(ns("table"))),
      picture.platform::ui_info_panel(
        system.file("about", "gen_event_count-text.html", package = "driveanalytics")
      )
    )

  )
}


#' Shiny server module for the Event count analysis
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_event_count
#' @export
gen_event_count_server <- function(id, df_server, df_pde_server, event_col, cohort_col, text=NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      gec <- shiny::reactive({
        gen_event_count(df_server(), df_pde_server(), event_col[[1]], cohort_col)
      })

      output$plot <- shiny::renderPlot({plot(gec())})
      output$table <- DT::renderDT(tabulate(gec(), "DT"))

    }
  )
}
