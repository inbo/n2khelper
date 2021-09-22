#' Test if an id exists in a given field of the table
#' @inheritParams odbc_get_id
#' @export
#' @importFrom assertthat assert_that is.count is.string
#' @importFrom DBI dbGetQuery
check_id <- function(value, variable, table, channel) {
  assert_that(is.count(value))
  assert_that(is.string(variable), noNA(variable))
  check_dbtable_variable(table = table, variable = variable, channel = channel)

  # nocov start
  sql <- paste("SELECT", variable, "FROM", table, "WHERE", variable, "=", value)
  selection <- dbGetQuery(
    conn = channel$con,
    statement = sql
  )
  if (nrow(selection) == 0) {
    return(FALSE)
  }
  if (nrow(selection) > 1) {
    warning(
      "The id '", value, "' in variable '", variable, "' of table '", table,
      "' exists in multiple rows"
    )
  }
  return(TRUE)
  # nocov end
}
