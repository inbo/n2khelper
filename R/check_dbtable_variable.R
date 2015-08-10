#' Check if a variable is available in a given table
#' @param error Indicates the behaviour when a variable is missing. Gives an error when error = TRUE (default). Return FALSE otherwise.
#' @inheritParams odbc_get_id
#' @export
#' @importFrom RODBC sqlColumns
#' @return TRUE when all variables are present in the table. 
check_dbtable_variable <- function(table, variable, schema = "dbo", channel, error = TRUE){
  table <- check_single_character(table, name = "table")
  variable <- check_character(x = variable, name = "variable", na.action = na.fail)
  if(length(variable) == 0){
    stop("'variable' must contain at least one value")
  }
  check_dbtable(table = table, schema = schema, channel = channel, error = TRUE)
  
  available <- sqlColumns(channel = channel, sqtable = table, schema = schema)$COLUMN_NAME
  check <- variable %in% available
  if(all(check)){
    return(TRUE)
  }
  if(error){
    stop(
      "Variable(s) missing from '", table, "': ",
      paste(variable[!check], collapse = ", ")
    )
  } else {
    return(FALSE)
  }
}
