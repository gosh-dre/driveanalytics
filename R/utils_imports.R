#' @importFrom grDevices colors
#' @importFrom stats na.omit setNames start
#' @importFrom utils head tail
#' @importFrom rlang := !!
utils_imports <- function() {}


#' @export
tabulate <- function(x, ...) UseMethod("tabulate")
tabulate.NULL <- function(x, ...) {return(NULL)}
head.NULL <- function(x, ...) {return(NULL)}
plot.NULL <- function(x, ...) {return(NULL)}
