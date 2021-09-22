#' Check if the object is a single POSIX
#'
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @param past Should the function throw an error when x is in the future?
#' Default is `FALSE`.
#' @return The function gives the single POSIX back.
#' It throws an error when the input is not a single character.
#' @export
#' @examples
#' check_single_posix(Sys.time())
check_single_posix <- function(x, name = "x", past = FALSE) {
  assert_that(inherits(x, "POSIXt"), length(x) == 1)
  assert_that(!past | x < Sys.time(), msg = paste(name, "is in the future."))
  return(x)
}
