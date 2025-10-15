#' Shiny UI module for the Calculate, tabulate and format the demographic characteristics by cohort
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family pde_cohort_characteristics
#' @export
pde_cohort_characteristics_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)

  shiny::tagList(

    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(title = "Table", shiny::htmlOutput(ns("tbl"))),
      picture.platform::ui_info_panel(
        system.file("about", "pde_cohort_characteristics-text.html", package = "driveanalytics")
      )
    )
  )
}


#' Shiny server module for the Calculate, tabulate and format the demographic characteristics by cohort
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param df_pde DESCRIPTION.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family pde_cohort_characteristics
#' @export
pde_cohort_characteristics_server <- function(id, df_pde) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      output$tbl <- shiny::renderUI({
        res <- pde_cohort_characteristics(df_pde())
        shiny::HTML(tabulate(res, 'html'))
      })


    }
  )
}
