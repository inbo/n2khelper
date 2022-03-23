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
check_dbtable <- function(table, schema = "public", channel, error = TRUE) {
  # nocov start
  table <- check_character(x = table, na_action = na.fail)
  assert_that(
    length(table) > 0, msg = "'table' must contain at least one value"
  )
  assert_that(is.string(schema))
  if (has_name(channel, "con")) {
    assert_that(inherits(channel$con, "DBIConnection"))
    this_channel <- channel$con
  } else {
    assert_that(inherits(channel, "DBIConnection"))
    this_channel <- channel
  }

  test <- sapply(
    table,
    function(x) {
      c(schema, x) %>%
        dbExistsTable(conn = this_channel)
    }
  )

  if (all(test) || !error) {
    return(all(test))
  }
  names(test)[!test] %>%
    paste(collapse = ", ") %>%
    sprintf(
      fmt = "Table(s) %s not found in schema %s on database %s",
      schema,
      dbGetInfo(this_channel)$dbname
    ) %>%
    stop(call. = FALSE)
  # nocov end
}
