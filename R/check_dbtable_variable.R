#' Check if a variable is available in a given table
#' @param error Indicates the behaviour when a variable is missing.
#' Gives an error when error = `TRUE` (default).
#' Return FALSE otherwise.
#' @param variable A vector with the names of the columns
#' @inheritParams odbc_get_id
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom DBI dbListFields
#' @importFrom stats na.fail
#' @return TRUE when all variables are present in the table.
check_dbtable_variable <- function(
  table, variable, schema = "public", channel, error = TRUE
) {
  assert_that(is.string(table), noNA(table))
  variable <- check_character(
    x = variable, name = "variable", na_action = na.fail
  )
  assert_that(
    length(variable) > 0, msg = "'variable' must contain at least one value"
  )
  check_dbtable(table = table, schema = schema, channel = channel, error = TRUE)
  # nocov start

  check <- variable %in%
    dbListFields(conn = channel, name = c(schema, table))
  if (all(check)) {
    return(TRUE)
  }
  if (error) {
    stop(
      "Variable(s) missing from '", table, "': ",
      paste(variable[!check], collapse = ", ")
    )
  } else {
    return(FALSE)
  }
  # nocov end
}
