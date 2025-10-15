#' Shiny server module to create age at event columns
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#' @param df_rdv Data frame to add column to
#' @param col Character label of event column
#' @param df_pde Demographics RDV containing DOBs - df_rdv used if not supplied
#'
#' @return Shiny moduleServer returning the updated data frame
#'
#' @family gen_age_at_event_server
#' @export
gen_age_at_event_server <- function(id, df_rdv, col, df_pde = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      reactive({

        if (is.null(df_rdv)) return(NULL)
        df_rdv_in <- df_rdv()
        if (is.null(df_pde))  df_pde_in <- NULL
        else                  df_pde_in <- df_pde()
        add_age_at_event(df_rdv_in, col, df_pde_in)

      })

    }
  )
}


add_age_at_event <- function(df_rdv, col, df_pde = NULL) {

  logger::log_debug("Started add_age_at_event")
  start.time <- Sys.time()

  # Exit if no input
  if (is.null(df_rdv)) return(NULL)

  # Combine DOBs if not already in the RDV
  if (!is.null(df_pde)) {
    df_rdv <- df_rdv %>%
      dplyr::left_join(
        dplyr::select(df_pde, project_id, birth_date),
        by = "project_id"
      )
  }

  # Age at function
  age_yrs_at <- function(cl, dob) {
    lubridate::time_length(difftime(as.Date(cl), dob), "years")
  }

  # Calculate age at column
  df_rdv_out <- df_rdv %>%
    dplyr::mutate(across(!!as.name(col), ~age_yrs_at(.x, birth_date), .names = "age_years_at_{.col}"))

  # Remove birth_date column, if necessary
  # TODO

  log_record_time_taken("add_age_at_event", start.time, "debug")


  # Return value
  df_rdv_out

}
