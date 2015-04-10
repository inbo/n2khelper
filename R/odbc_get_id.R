#' Get the id of the mathcing records
#' @export
#' @param table the name of the 
#' @param variable the name of the variable
#' @param value the content of the variable
#' @inheritParams connect_result
#' @importFrom RODBC sqlQuery odbcClose
odbc_get_id <- function(table, variable, value, develop = TRUE){
  value <- check_character(value, name = "value")
  if(length(value) == 0){
    stop("at least one value is needed")
  }
  channel <- connect_result(develop = develop)
  check_dbtable_variable(table = table, variable = variable, channel = channel)
  if(length(value) != length(variable)){
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
  odbcClose(channel)
  return(id)
}
