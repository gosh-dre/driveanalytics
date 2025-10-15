#' Shiny UI module for the Analysis of the periodicity of event recurrence
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_recurrence_analysis
#' @export
gen_recurrence_analysis_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(

    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(title = "Plot",
                      shiny::fluidRow(
                        shiny::selectInput(
                          ns("event"),
                          "Select event",
                          multiple =  TRUE,
                          choices = character(0)
                          ),
                        shiny::column(
                          7,
                          offset = 1,
                          shiny::checkboxInput(
                            ns("plot_options"),
                            label = "Outliers",
                            value = FALSE
                          )
                        ),
                        ),
                      shiny::plotOutput(ns("plot"))
                      ),
      shiny::tabPanel(
        title = "Table",
        shiny::div(DT::dataTableOutput(ns("table")), style = "font-size:80%"))#,


      # picture.platform::ui_info_panel(
      #   system.file("about", "gen_recurrence_analysis-text.Rmd", package = "driveanalytics")
      # )
    )
  )
}

#' Shiny server module for the Analysis of the periodicity of event recurrence
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_recurrence_analysis
#' @export
gen_recurrence_analysis_server <- function(id, df_server, colm, cohort_col = "cohort") {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- shiny::NS(id)

      gra <- shiny::reactive({
        gen_recurrence_analysis(df_server(), colm, cohort_col)
      })

      shiny::observe({

        if (is.null(df_server()) || nrow(df_server()) == 0) return()

        df_names <- df_server() %>%
          picture.platform::add_codename_col(colm, "names") %>%
          dplyr::transmute(code = !!as.name(colm), names, project_id) %>%
          dplyr::distinct(names, code)

        choices_list <- df_names$code
        names(choices_list) <- df_names$names

        selected_type <- ifelse(input$event == "",
                                  choices_list[1],
                                  input$event)

        shiny::updateSelectInput(session, "event",
                                 choices = choices_list,
                                 selected = selected_type)

      })

      output$plot <- shiny::renderPlot({
        req(input$event)
        plot(gra(), input$event, input$plot_options)
      })

      output$table <- DT::renderDataTable({
        DT::datatable(tabulate(gra()), escape=FALSE, options = list(scrollX = TRUE))
      })

    }
  )
}
