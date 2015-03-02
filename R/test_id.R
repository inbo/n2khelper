#' Test is an id exists in a given field of the table
#' @param value the value of the id
#' @param field the field in which holds the id
#' @param table the table in which to look for the id
#' @param channel An open ODBC channel
#' @export
#' @importFrom RODBC sqlQuery
test_id <- function(value, field, table, channel){
  if(class(channel) != "RODBC"){
    stop("channel is not an ODBC connection")
  }
  sql <- paste("SELECT", field, "FROM", table, "WHERE", field, "=", value)
  selection <- sqlQuery(channel = channel, query = sql)
  if(class(selection) != "data.frame"){
    if(length(grep("Invalid object name", selection))){
      stop("The table '", table, "' doesn't exists in the ODBC data source")
    }
    if(length(grep("Invalid column name", selection)) > 0){
      stop("The field '", field, "' doesn't exists in table '", table, "'")
    }
  }
  
  if(nrow(selection) == 0){
    return(FALSE)
  } else {
    if(nrow(selection) > 1){
      warning("The id '", value, "' in field '", field, "' of table '", table, "' exists in multiple rows")
    }
    return(TRUE)
  }
}
