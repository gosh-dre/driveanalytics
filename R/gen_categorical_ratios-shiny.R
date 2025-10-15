#' Shiny UI module for the Simple analysis of the ratios of categorical variables within an RDV
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_categorical_ratios
#' @export
gen_categorical_ratios_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(title = "Plot",
                      shiny::plotOutput(ns("plot")),
                      shiny::textOutput(ns("test"))),
      picture.platform::ui_info_panel(
        system.file("about", "gen_categorical_ratios-text.html", package = "driveanalytics")
      )
    )

  )
}


#' Shiny server module for the Simple analysis of the ratios of categorical variables within an RDV
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param df_server DESCRIPTION.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_categorical_ratios
#' @export
gen_categorical_ratios_server <- function(id, df_server, col, stratify_by = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      output$plot <- shiny::renderPlot({
        gen_categorical_ratios(df_server(), col, stratify_by)$plt
      })

      output$test <- shiny::renderText({
        gen_categorical_ratios(df_server(), col, stratify_by)$test_res
      })

    }
  )
}
