#' Shiny UI module for distribution plots
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_distribution_plots
#' @export
gen_distribution_plots_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(
        title = "Plot",
        shiny::fluidRow(
          id = ns("type_select_wrap"),
          shiny::column(6, shiny::selectizeInput(ns("transform_select"), "Select transformation", choices = c("No transformation", "Log"), selected = "No transformation")),
          shiny::column(6, shiny::selectizeInput(ns("type_select"), "Select type", choices = NULL))
        ),
        shiny::plotOutput(ns("plot")),
        shiny::fluidRow(
          shiny::column(3, shiny::sliderInput(ns("density_bw"), "Smoothing bandwidth", min = 0.1, max = 10, value = 1, step = 0.1))
        ),
        shiny::br(),
        shiny::tableOutput(ns("plotn")),
        shiny::br(),
        shiny::textOutput(ns("testResult"))
      ),
      picture.platform::ui_info_panel(
        system.file("about", "gen_distribution_plots-text.html", package = "driveanalytics")
      )
    )
  )
}


#' Shiny server module for distribution plots
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param df_rdv Data frame.
#' @param col Character label of column to plot.
#' @param stratify_by Character label of grouping column.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_distribution_plots
#' @export
gen_distribution_plots_server <- function(id, df_rdv, col, stratify_by, filter_col = NULL, filter_val = NULL, calc_length = FALSE, calc_diff = FALSE, filter_condition = NULL, xlab = NULL) {

  # ns <- shiny::NS(id)

  shiny::moduleServer(
    id,
    function(input, output, session) {

      gdp <- shiny::reactive({

        df <- df_rdv()
        if (!is.null(filter_col)) {
          df <- df %>%
            dplyr::filter(!!as.name(filter_col) == input$type_select)
        }
        gen_distribution_plots(df_rdv = df,
                               col = col,
                               stratify_by = stratify_by,
                               calc_length = calc_length,
                               calc_diff = calc_diff,
                               filter_condition = filter_condition,
                               transform = input$transform_select,
                               density_bw = input$density_bw,
                               xlab = xlab)
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

        # Decide whether to show or hide the select and transformation option
        if (is.null(df_rdv())) {
          shinyjs::hide(id = "type_select_wrap")
          return()
        } else {
          if (is.null(filter_col)) {
            shinyjs::hide(id = "type_select")
            return()
          } else {
            shinyjs::show(id = "type_select")
          }
        }

        df_choices <- df_rdv() %>%
          dplyr::select(dplyr::any_of(c(filter_col, col))) %>%
          tidyr::drop_na(dplyr::any_of(col)) %>%
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
        grid::grid.draw(gdp()$plot)
      })

      output$plotn <- shiny::renderTable({
        gdp()$tbl_n
      })

      output$testResult <- shiny::renderText({
        gdp()$test_result
      })

    }
  )
}
