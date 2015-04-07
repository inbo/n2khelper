#' Get the id of a data source
#' @export
#' @param data.source.name the description of the data source
#' @inheritParams connect_result
#' @importFrom RODBC sqlQuery odbcClose
get_datasource_id <- function(data.source.name, develop = TRUE){
  data.source.name <- check_single_character(data.source.name)
  
  channel <- connect_result(develop = develop)
  sql <- paste0("
    SELECT
      ID
    FROM
      Datasource
    WHERE
      Description = '", data.source.name, "'" 
  )
  id <- sqlQuery(channel = channel, query = sql)$ID
  odbcClose(channel)
  if(length(id) == 0){
    stop("'", data.source.name, "' not found in database")
  }
  if(length(id) > 1){
    warning("Multiple id's found for '", data.source.name, "'")
  }
  return(id)
}
