#' Check if the object is a single logical
#'
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @return The function gives the single logical back. it throws an error when the input is not a single logical.
#' @export
#' @examples
#' check_single_logical(TRUE)
check_single_logical <- function(x, name = "x"){
  name <- check_single_character(x = name, name = "name")
  if (class(x) != "logical") {
    stop(name, " must be logical")
  }
  if (length(x) != 1) {
    stop(name, " must be a single logical")
  }
  if (is.na(x)) {
    stop(name, " must be TRUE or FALSE")
  }
  return(x)
}
