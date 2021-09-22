#' Check if a data.frame contains variables
#'
#' @param df the \code{data.frame} to check
#' @param variable either a character vector with the names of the variable to
#'    check or a named list. The names of the list must match the names of the
#'    required variables in the data.frame. The elements of the list contain the
#'    accepted classes for each varaible.
#' @param name the name of the \code{data.frame} to use in the error message
#' @param force_na check the class of variables with all NA
#' @param error When TRUE (default), the function returns an error when a
#'    variable is missing. Otherwise it returns a warning.
#' @return The function returns TRUE when all variables are present. If returns
#'    FALSE when a variable is missing and \code{error = FALSE}.
#' @export
#' @importFrom dplyr %>%
#' @examples
#' check_dataframe_variable(
#'  df = data.frame(a = integer(0)),
#'  variable = "a"
#' )
#' check_dataframe_variable(
#'  df = data.frame(a = integer(0)),
#'  variable = list(a = c("integer", "numeric"))
#' )
#' @importFrom assertthat assert_that is.string is.flag noNA
check_dataframe_variable <- function(
  df, variable, name = "df", force_na = FALSE, error = TRUE
) {
  assert_that(is.string(name))
  assert_that(inherits(df, "data.frame") | inherits(df, "matrix"))
  assert_that(is.flag(force_na))
  assert_that(noNA(force_na))
  assert_that(is.flag(error))
  assert_that(noNA(error))
  assert_that(is.list(variable) | is.character(variable))

  if (inherits(variable, "list")) {
    required_class <- variable
    variable <- names(required_class)
  } else {
    required_class <- NULL
  }

  assert_that(length(variable) > 0)
  assert_that(noNA(variable))

  available <- variable %in% colnames(df)
  if (!all(available)) {
    missing.var <- paste(variable[!available], collapse = ", ")
    if (error) {
      stop("Variables missing in ", name, ": ", missing.var)
    } else {
      warning("Variables missing in ", name, ": ", missing.var)
      return(FALSE)
    }
  }

  if (is.null(required_class)) {
    return(TRUE)
  }

  if (force_na) {
    all_na <- rep(FALSE, length(variable))
  } else {
    all_na <- sapply(
      df[, variable],
      function(x) {
        all(is.na(x))
      }
    )
  }
  correct_class <- sapply(df[, variable[!all_na], drop = FALSE], class)
  correct_class <- sapply(
    seq_along(correct_class),
    function(i) {
    any(correct_class[[i]] %in% required_class[!all_na][[i]])
    }
  )
  if (!all(correct_class)) {
    wrong_class <- correct_class[!correct_class]
    wrong_class <- sapply(wrong_class, paste, collapse = "', '")
    expected_class <- required_class[!all_na][names(wrong_class)]
    expected_class <- sapply(expected_class, paste, collapse = "', '")
    sprintf(
      "\n%s: got '%s', expected '%s'", names(wrong_class), wrong_class,
      expected_class
    ) %>%
      paste(collapse = "") %>%
      sprintf(fmt = "Wrong class for following variable(s)%s") %>%
      stop()
  }
  return(TRUE)
}
