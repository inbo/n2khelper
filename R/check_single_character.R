#' Check if the object is a single character
#'
#' Factors are converted to character.
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @return The function gives the single character back. it throws an error when the input is not a single character.
#' @export
#' @examples
#' check_single_character("20")
check_single_character <- function(x, name = "x"){
  x <- check_character(x = x, name = name, na.action = na.fail)
  if (length(x) != 1) {
    stop(name, " must be a single character")
  }
  return(x)
}
