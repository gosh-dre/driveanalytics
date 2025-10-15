ppl_wrangle_delta_datetimes_ui <- function(id) {}


ppl_wrangle_delta_datetimes_server <- function(
  id, df_rdv_server, df_pde_server = NULL, from_datetime_col, to_datetime_col,
  units, output_col, min_dt = -Inf, max_dt = Inf
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      #' Return the reactive for the filtered RDV to use in an analytics pipeline
      return(shiny::reactive({

        req(df_rdv_server())

        if(nrow(df_rdv_server()) == 0) return(df_rdv_server())

        df_rdv_server() %>%
          # Merge the demographics birth_date if provided
          {
            if(from_datetime_col == "birth_date" && !is.null(df_pde_server))
              dplyr::left_join(
                .,
                dplyr::select(df_pde_server(), "cohort_id", "birth_date"),
                by = "cohort_id"
              )
            else
              .
          } %>%
          # Compute the output column
          dplyr::mutate(
            !!output_col := pmax(0,
                                 lubridate::interval(
                                   !!as.name(from_datetime_col),
                                   !!as.name(to_datetime_col)
                                 ) / lubridate::years(1))
          ) %>%
          # Drop any values outside the required range
          dplyr::filter(
            !!as.name(output_col) >= min_dt,
            !!as.name(output_col) <= max_dt
          )

      }))

    }
  )
}
