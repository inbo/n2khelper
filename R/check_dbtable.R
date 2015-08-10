#' Check if a table is available in a given ODBC connection
#' @param error Indicates the behaviour when a table is missing. Gives an error
#'    when error = TRUE (default). Return FALSE otherwise.
#' @param schema The schema. Defaults to 'dbo'
#' @inheritParams odbc_get_id
#' @export
#' @importFrom RODBC sqlTables
#' @return TRUE when all tables are present in the ODBC connection.
check_dbtable <- function(table, schema = "dbo", channel, error = TRUE){
  table <- check_character(x = table, name = "table", na.action = na.fail)
  if (length(table) == 0) {
    stop("'table' must contain at least one value")
  }
  schema <- check_single_character(x = schema, name = "schema")
  if (class(channel) != "RODBC") {
    stop("channel is not an ODBC connection")
  }

  available <- sqlTables(channel = channel, schema = schema)$TABLE_NAME
  check <- table %in% available
  if (all(check)) {
    return(TRUE)
  }
  if (error) {
    stop(
      "Table(s) missing: ",
      paste(table[!check], collapse = ", ")
    )
  } else {
    return(FALSE)
  }
}
