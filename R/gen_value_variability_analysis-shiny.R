#' Shiny UI module for the Analysis of variability for value-based data
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_value_variability_analysis
#' @export
gen_value_variability_analysis_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(
        title = "Plot",
        shiny::fluidRow(
          id = ns("type_select_wrap"),
          shiny::column(6, shiny::selectizeInput(ns("type_select"), "Select type", choices = NULL)),
          shiny::column(6, shiny::selectizeInput(ns("period_select"), "Aggregation period", choices = c("day", "week", "month", "year")))
        ),
        shiny::plotOutput(ns("plot"))
      ),
      picture.platform::ui_info_panel(
        system.file("about", "gen_value_variability_analysis-text.html", package = "driveanalytics")
      )
    )

  )
}


#' Shiny server module for the Analysis of variability for value-based data
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_value_variability_analysis
#' @export
gen_value_variability_analysis_server <- function(
  id, df_rdv, value_col, datetime_col, filter_col = NULL,
  cohort_col = "cohort"
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      gvva <- shiny::reactive({

        df <- df_rdv()
        if (!is.null(filter_col)) {
          df <- df %>%
            dplyr::filter(!!as.name(filter_col) == input$type_select)
        }
        gen_value_variability_analysis(
          df, value_col, datetime_col, cohort_col = cohort_col,
          time_period = input$period_select
        )
      })


      shiny::observeEvent(input$type_select, once = TRUE, {
        # Set the type select options on the first open
        # This is important for dynamically rendered modules where the UI may
        # not be initialised the first time that data is pushed into the server
        # functions
        updateTypeSelect()
      })


      shiny::observeEvent(df_rdv(), {
        updateTypeSelect()
      })


      updateTypeSelect <- function() {

        # Don't set the input options if its not an initialised ui
        is_initialised <- !is.null(input$type_select)
        if(!is_initialised) return()

        # Decide whether to show or hide the select option
        if (is.null(filter_col) || is.null(df_rdv())) {
          shinyjs::hide(id = "type_select_wrap")
          return()
        } else {
          shinyjs::show(id = "type_select_wrap")
        }

        df_choices <- df_rdv() %>%
          dplyr::select(!!as.name(filter_col), !!as.name(value_col)) %>%
          tidyr::drop_na(!!as.name(value_col)) %>%
          dplyr::group_by(!!as.name(filter_col)) %>%
          dplyr::count() %>%
          dplyr::arrange(desc(n)) %>%
          dplyr::mutate(names = paste0(!!as.name(filter_col), " (n = ", n, ")"))

        choices_list <- df_choices %>% dplyr::pull(!!as.name(filter_col))
        names(choices_list) <- df_choices$names

        shiny::updateSelectizeInput(session, "type_select",
                                    choices = choices_list,
                                    selected = choices_list[1],
                                    server = TRUE)
      }

      output$plot <- shiny::renderPlot({
        grid::grid.draw(gvva()$plot)
      },
      height = 800)

    }
  )
}
