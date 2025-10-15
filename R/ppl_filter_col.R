#' @export
#' @family ppl_filter_col
ppl_filter_col_ui <- function(id) {

  ns <- shiny::NS(id)
  shiny::column(12, shiny::selectInput(ns("filter_select_val"), "Event", c()))

}


#' @export
#' @family ppl_filter_col
ppl_filter_col_server <- function(id, df_rdv_server, filter_col, filter_opts, filter_expr = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {


      #' Update options on the first time the UI is rendered
      #' This is important for dynamically rendered modules where the UI may
      #' not be initialised the first time that data is pushed into the server
      #' functions
      shiny::observeEvent(input$filter_select_val, once = TRUE, update_fs_options())


      #' Update options on new input RDV
      shiny::observeEvent(df_rdv_server(), update_fs_options())


      #' Function to update the selectInput
      update_fs_options <- function() {

        options <- df_rdv_server() %>%
          dplyr::distinct(!!as.name(filter_col)) %>%
          dplyr::filter(!!as.name(filter_col) %in% filter_opts) %>%
          dplyr::pull(1)

        shiny::updateSelectInput(
          session, "filter_select_val", choices = options, selected = options[[1]]
        )

      }


      #' Return the reactive for the filtered RDV to use in an analytics pipeline
      return(shiny::reactive({

        # browser()

        if (is.null(input$filter_select_val)) return(tibble::tibble())

        # browser()

        df_rdv_server() %>%
          dplyr::filter(!!as.name(filter_col) == input$filter_select_val) %>%
          {
            if (!is.null(filter_expr)) {
              dplyr::filter(eval(rlang::parse_expr(filter_condition)))
            } else {
              .
            }
          }

      }))
    }
  )
}
