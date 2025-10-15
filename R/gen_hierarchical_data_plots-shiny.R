#' Shiny UI module for the Generate plots for hierarchical data
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param title String; title of tabBox.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_hierarchical_data_plots
#' @export
gen_hierarchical_data_plots_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tagList(
      bs4Dash::tabBox(
        title = title,
        side = "right",
        width = 12,
        shiny::tabPanel(
          title = "Plot",
          plotly::plotlyOutput(ns("plot"), height = "500px")),
          picture.platform::ui_info_panel(
            system.file("about", "gen_hierarchical_data_plots-text.html", package = "driveanalytics")
          )
      )
    )
  )
}

#' Shiny server module for the Generate plots for hierarchical data
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param df_server Reactive server function providing a dataframe.
#' @param group_cols Character; name of columns in df_rdv represented by a tile
#' ordered from higher to lower level.
#' @param size_col Character; name of column in df_rdv to use for count.
#' @param stratify_by Character; name of column in df_rdv to use for other grouping.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_hierarchical_data_plots
#' @export
gen_hierarchical_data_plots_server <- function(id, df_server, group_cols, size_col, stratify_by) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ghdp <- shiny::reactive({
        gen_hierarchical_data_plots(df_server(), group_cols, size_col, stratify_by)
      })

      output$plot <- plotly::renderPlotly({
        ghdp()$plt
      })
    }
  )
}
