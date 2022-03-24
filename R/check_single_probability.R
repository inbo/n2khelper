#' Check if the object is a single probability
#'
#' @inheritParams check_character
#' @return The function gives the single probability back.
#' It throws an error when the input is not a single probability.
#' @export
#' @importFrom assertthat assert_that is.number noNA
#' @examples
#' check_single_probability(0.5)
check_single_probability <- function(x, name = "x") {
  assert_that(is.number(x), msg = paste(name, "must be a single numeric"))
  assert_that(noNA(x), msg = paste(name, "cannot be NA"))
  assert_that(
    x >= 0, x <= 1, msg = paste(name, "must be a value between 0 and 1")
  )
  return(x)
}
