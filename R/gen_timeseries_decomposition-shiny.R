#' Shiny UI module for the Generic decomposition of a timeseries by Prophet
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_timeseries_decomposition
#' @export
gen_timeseries_decomposition_ui <- function(id) {

  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      title = NULL,
      width = 12,
      shiny::tabPanel(
        title = "Plot",
        shiny::plotOutput(
          ns("ts_plot"),
          height = "600px"
        )
      ),
      picture.platform::ui_info_panel(
        system.file("about", "gen_timeseries_decomposition-text.html", package = "driveanalytics")
      )
    )

  )
}


#' Shiny server module for the Generic decomposition of a timeseries by Prophet
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_timeseries_decomposition
#' @export
gen_timeseries_decomposition_server <- function(id, df_server) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      gtsd <- reactive({gen_timeseries_decomposition(df_server())})

      output$ts_plot <- shiny::renderPlot({
        gtsd()
      })

    }
  )
}
