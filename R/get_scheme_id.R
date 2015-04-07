#' Get the id of a scheme
#' @export
#' @param scheme.name the name of the scheme
#' @inheritParams connect_result
#' @importFrom RODBC sqlQuery odbcClose
get_scheme_id <- function(scheme.name, develop = TRUE){
  scheme.name <- check_single_character(scheme.name)
  
  channel <- connect_result(develop = develop)
  sql <- paste0("
    SELECT
      ID
    FROM
      Scheme
    WHERE
      Description = '", scheme.name, "'" 
  )
  id <- sqlQuery(channel = channel, query = sql)$ID
  odbcClose(channel)
  if(length(id) == 0){
    stop("'", scheme.name, "' not found in database")
  }
  if(length(id) > 1){
    warning("Multiple id's found for '", scheme.name, "'")
  }
  return(id)
}
