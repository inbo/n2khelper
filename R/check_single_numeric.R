#' Check if the object is a single numeric
#'
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @return The function gives the single numeric back. it throws an error when the input is not a single numeric.
#' @export
#' @examples
#' check_single_numeric(0.5)
check_single_numeric <- function(x, name = "x"){
  name <- check_single_character(x = name, name = "name")
  if (!class(x) %in% c("numeric", "integer")) {
    stop(name, " must be numeric")
  }
  if (length(x) != 1) {
    stop(name, " must be a single numeric")
  }
  return(x)
}
