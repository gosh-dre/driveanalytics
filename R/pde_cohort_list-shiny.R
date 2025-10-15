#' Shiny UI module for the List patients by cohort
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family pde_cohort_list
#' @export
pde_cohort_list_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      width = 12,
      shiny::tabPanel(
        title = "Patient List",
        DT::dataTableOutput(ns("table"))
      ),
      picture.platform::ui_info_panel(
        system.file("about", "pde_cohort_list-text.html", package = "driveanalytics")
      )
    )

  )
}


#' Shiny server module for the List patients by cohort
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family pde_cohort_list
#' @export
pde_cohort_list_server <- function(id, df_pde) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      output$table <- DT::renderDT({pde_cohort_list(df_pde())})

    }
  )
}
