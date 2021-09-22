#' Check if the object is a single probability
#'
#' @param x the object to check
#' @return The function gives the single probability back.
#' It throws an error when the input is not a single probability.
#' @export
#' @importFrom assertthat assert_that is.number noNA
#' @examples
#' check_single_probability(0.5)
check_single_probability <- function(x) {
  assert_that(is.number(x), noNA(x), x > 0, x < 1)
  return(x)
}
