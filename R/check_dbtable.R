#' Check if a table is available in a given ODBC connection
#' @param error Indicates the behaviour when a table is missing. Gives an error
#'    when error = TRUE (default). Return FALSE otherwise.
#' @param schema The schema. Defaults to 'public'
#' @inheritParams odbc_get_id
#' @export
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr %>%
#' @importFrom DBI dbExistsTable dbGetInfo
#' @importFrom stats na.fail
#' @return TRUE when all tables are present in the ODBC connection.
check_dbtable <- function(table, schema = "public", channel, error = TRUE){
  table <- check_character(x = table, name = "table", na.action = na.fail)
  if (length(table) == 0) {
    stop("'table' must contain at least one value")
  }
  assert_that(is.string(schema))
  assert_that(inherits(channel$con, "DBIConnection"))
  # nocov start

  test <- sapply(
    table,
    function(x){
      c(schema, x) %>%
        dbExistsTable(conn = channel)
    }
  )
  if (all(test)) {
    return(TRUE)
  }
  if (error) {
    names(test)[!test] %>%
      paste(collapse = ", ") %>%
      sprintf(
        fmt = "Table(s) %s not found in schema %s on database %s",
        schema,
        dbGetInfo(channel)$dbname
      ) %>%
      stop(call. = FALSE)
  } else {
    return(FALSE)
  }

  # nocov end
}
