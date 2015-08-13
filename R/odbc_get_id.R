#' Get the id of the matching records
#' @export
#' @param table The name of the table
#' @param variable A vector with the names of the columns
#' @param value the content of the variable
#' @param schema The schema of the table. Defaults to dbo
#' @param channel the open ODBC channel
#' @importFrom RODBC sqlQuery
odbc_get_id <- function(table, variable, value, schema = "dbo", channel){
  value <- check_character(value, name = "value")
  if (length(value) == 0) {
    stop("at least one value is needed")
  }
  check_dbtable_variable(table = table, variable = variable, channel = channel)
  # nocov start
  if (length(value) != length(variable)) {
    stop("the number of values doesn't match the number of variables")
  }

  where <- paste0(variable, " = '", value, "'", collapse = " AND \n")
  sql <- paste0("
    SELECT
      ID
    FROM
      ", table, "
    WHERE
      ", where
  )
  id <- sqlQuery(channel = channel, query = sql)$ID
  return(id) # nocov end
}
