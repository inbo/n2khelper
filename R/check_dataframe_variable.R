#' Check if a data.frame contains variables
#'
#' @param df the \code{data.frame} to check
#' @param variable either a character vector with the names of the variable to
#'    check or a named list. The names of the list must match the names of the
#'    required variables in the data.frame. The elements of the list contain the
#'    accepted classes for each varaible.
#' @param name the name of the \code{data.frame} to use in the error message
#' @param error When TRUE (default), the function returns an error when a
#'    variable is missing. Otherwise it returns a warning.
#' @return The function returns TRUE when all variables are present. If returns
#'    FALSE when a variable is missing and \code{error = FALSE}.
#' @export
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
check_dataframe_variable <- function(df, variable, name = "df", error = TRUE){
  assert_that(is.string(name))
  assert_that(inherits(df, "data.frame") | inherits(df, "matrix"))
  assert_that(is.flag(error))
  assert_that(noNA(error))
  assert_that(is.list(variable) | is.character(variable))

  if (inherits(variable, "list")) {
    required.class <- variable
    variable <- names(required.class)
  } else {
    required.class <- NULL
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

  if (is.null(required.class)) {
    return(TRUE)
  }

  all.NA <- sapply(
    df[, variable],
    function(x){
      all(is.na(x))
    }
  )
  current.class <- sapply(df[, variable[!all.NA]], class)
  correct.class <- sapply(seq_along(current.class), function(i){
    any(current.class[[i]] %in% required.class[!all.NA][[i]])
  })
  if (!all(correct.class)) {
    wrong.class <- current.class[!correct.class]
    wrong.class <- sapply(wrong.class, paste, collapse = "', '")
    expected.class <- required.class[!all.NA][names(wrong.class)]
    expected.class <- sapply(expected.class, paste, collapse = "', '")
    stop(
      "Wrong class for following variable(s)\n",
      paste0(
        names(wrong.class), ": got '", wrong.class,
        "', expected '", expected.class, "'\n",
        collapse = ", "
      )
    )
  }
  return(TRUE)
}
