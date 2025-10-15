# library(shinyTree) # This seems to be necessary - though bad practice

#' Shiny UI module for the Simple time series analysis for a selected column in any RDV
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_timeseries_analysis
#' @export
gen_timeseries_analysis_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bs4Dash::tabBox(
      title = title,
      side = "right",
      width = 12,
      shiny::tabPanel(
        title = "Plot",
        plotly::plotlyOutput(ns("plot")),
        shiny::fluidRow(
          shiny::column(
            6,
            # shiny::div(shiny::tags$label("Select timeseries")),
            # bs4Dash::box(
            #   width = 12, collapsible = FALSE,
              # class = "shiny-tree-box",
              shiny::selectizeInput(ns("type_select"), "Select type", choices = NULL)
              # shinyTree::shinyTree(ns("tree"), stripes = TRUE, multiple = TRUE, animation = TRUE, checkbox = TRUE, search = TRUE, themeIcons = FALSE)
            # )
          ),
          shiny::column(
            2,
            shiny::div(shiny::tags$label("Data grouping")),
            # shiny::checkboxInput(ns("groupVar1"), "Group on VAR1", TRUE),
            shiny::checkboxInput(ns("overlayCohorts"), "Overlay cohorts")
          ),
          shiny::column(
            2, shiny::selectInput(
              ns("horAxis"), "Select horizontal axis",
              c("Years since entering cohort", "Years since birth",
                "Days since first event", "Event date", "Event number"))
          ),
          shiny::column(
            2, shiny::selectInput(
              ns("selectAggregation"), "Data aggregation", c("None", "Constant centiles", "Time varying centiles"))
          )
        )),
      shiny::tabPanel(title = "Numerical Results Table",
                      shiny::div(DT::dataTableOutput(ns("table_num")),
                                 style = "font-size:80%")),
      shiny::tabPanel(title = "Categorical Results Table",
                      shiny::div(DT::dataTableOutput(ns("table_cat")),
                                 style = "font-size:80%")),
      picture.platform::ui_info_panel(
        system.file("about", "gen_timeseries_analysis-text.html", package = "driveanalytics")
      )
    )
  )
}


#' Shiny server module for the Simple time series analysis for a selected column
#' in any RDV
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id character; the namespace for the module instance.
#' @param event_col character; the name of the event column; required.
#' @param series_col character; the name of the time series column; required.
#' @param datetime_col character; the name of the event datetime column; required.
#' @param stratify_by character; name of column to stratify by; required.
#' @param group_by character; name of additional grouping column.
#' @param type character; name of group_by datatype column.
#' @param units character; name of the unit column.
#' @param ref_range character vector; names of reference range columns.
#' @param df_server Reactive server function providing a dataframe.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_timeseries_analysis
#' @export
gen_timeseries_analysis_server <- function(
  id, df_server, df_pde_server, value_col, datetime_col, cohort_col, grouping_col,
  value_type_col = NULL, units_col = NULL, ref_range_col = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      updateCheckboxInput(session, "groupVar1", label = paste("Group on", grouping_col))

      shiny::observeEvent(input$type_select, once = TRUE, {
        # Set the type select options on the first open
        # This is important for dynamically rendered modules where the UI may
        # not be initialised the first time that data is pushed into the server
        # functions
        updateTypeSelect()
      })


      shiny::observeEvent(df_server(), {
        updateTypeSelect()
      })


      updateTypeSelect <- function() {

        # Don't set the input options if its not an initialised ui
        is_initialised <- !is.null(input$type_select)
        if(!is_initialised) return()

        df_choices <- df_server() %>%
          dplyr::select(!!as.name(grouping_col), !!as.name(value_col)) %>%
          tidyr::drop_na(!!as.name(value_col)) %>%
          dplyr::group_by(!!as.name(grouping_col)) %>%
          dplyr::count() %>%
          dplyr::arrange(desc(n)) %>%
          dplyr::mutate(names = paste0(!!as.name(grouping_col), " (n = ", n, ")"))

        choices_list <- df_choices %>% dplyr::pull(!!as.name(grouping_col))
        names(choices_list) <- df_choices$names

        shiny::updateSelectizeInput(session, "type_select",
                                    choices = choices_list,
                                    selected = choices_list[1],
                                    server = TRUE)
      }


      gta <- shiny::reactive({

        df_filtered <- df_server()
        if (!is.null(grouping_col)) {
          df_filtered <- df_filtered %>%
            dplyr::filter(!!as.name(grouping_col) == input$type_select)
        }

        # Run the timeseries analysis and return the result
        gen_timeseries_analysis(
          df_filtered, df_pde_server(), grouping_col, value_col, datetime_col,
          cohort_col, value_type_col, units_col, ref_range_col, input$horAxis,
          overlay_cohorts = input$overlayCohorts,
          aggregate_cohorts = input$selectAggregation
        )

      })


      output$plot <- plotly::renderPlotly({
        if (is.null(gta()$plt)) return(NULL)
        plotly::ggplotly(gta()$plt)  %>%
          plotly::layout(legend = list(
            # title = "",
            orientation = "h",
            xanchor = "left", x = 0,
            yanchor = "bottom", y = 1.1
          ))
      })

      output$table_num <- DT::renderDataTable({
        DT::datatable(gta()$df, escape=FALSE, options = list(scrollX = TRUE))
      })

      output$table_cat <- DT::renderDataTable({
        DT::datatable(gta()$df_cat, escape=FALSE, options = list(scrollX = TRUE))
      })

    }
  )
}
