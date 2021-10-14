#' Check if the object is a character
#'
#' Factors are converted to character.
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @param na_action `stats::na.fail()` throws an error in case of `NA`
#' (default).
#' `stats::na.omit()` will return `x` without the `NA` values.
#' `stats::na.pass()` will return `x` with the `NA` values.
#' @return The function gives the character back.
#' It throws an error when the input is not a character.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stats na.fail
#' @examples
#' check_character(c("20", "b"))
check_character <- function(x, name = "x", na_action = na.fail) {
  assert_that(is_chartor(x), msg = paste(name, "must be character"))
  x <- na_action(x)
  if (is.factor(x)) {
    return(levels(x)[x])
  }
  return(x)
}
