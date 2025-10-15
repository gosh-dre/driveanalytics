#' Helper functions for logging
#'
#' Description
#'
#' @param name str name of function
#' @param start_time datetime start time
#'
#' @family utils_logger
#' @export
log_record_time_taken <- function(name, start_time, log_level="info"){
  end.time <- Sys.time()
  time.taken <- end.time - start_time
  end_statement <- glue::glue("Finished {name}: {time.taken}")
  if(log_level == "debug"){
    logger::log_debug(end_statement)
  } else{
    logger::log_info(end_statement)
  }
}

#' Simple analysis of the ratios of categorical variables within an RDV
#'
#' Description
#'
#' @param name str name of function
#' @param start_time datetime start time
#'
#' @family utils_logger
#' @export
log_start_function_return_time <- function(name, log_level="info"){
  if(log_level == "debug"){
    logger::log_debug(glue::glue("Started {name}"))
  } else{
    logger::log_info(glue::glue("Started {name}"))
  }
  return(Sys.time())
}
