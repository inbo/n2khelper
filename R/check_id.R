#' Test if an id exists in a given field of the table
#' @inheritParams odbc_get_id
#' @export
#' @importFrom RODBC sqlQuery
check_id <- function(value, variable, table, channel){
  value <- check_single_strictly_positive_integer(value, name = "value")
  variable <- check_single_character(variable, name = "variable")
  check_dbtable_variable(table = table, variable = variable, channel = channel)

  sql <- paste("SELECT", variable, "FROM", table, "WHERE", variable, "=", value)
  selection <- sqlQuery(channel = channel, query = sql)
  if (class(selection) != "data.frame") {
    if (length(grep("Invalid column name", selection)) > 0) {
      stop(
        "The variable '", variable, "' doesn't exists in table '", table, "'"
      )
    }
    if (length(grep("Conversion failed", selection))) {
      stop(paste(selection, collapse = "\n"))
    }
  }

  if (nrow(selection) == 0) {
    return(FALSE)
  } else {
    if (nrow(selection) > 1) {
      warning(
        "The id '", value, "' in variable '", variable, "' of table '", table,
        "' exists in multiple rows"
      )
    }
    return(TRUE)
  }
}
