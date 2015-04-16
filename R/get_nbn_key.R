#' Get the NBN key of a species
#' @param language The language to use. Defaults to "la" 'scientific name"
#' @inheritParams connect_result
#' @export
#' @importFrom RODBC sqlQuery odbcClose
get_nbn_key <- function(language = "la", develop = TRUE){
  language <- check_single_character(language)
  
  channel <- odbc_connect("NBN data", develop = develop)
  sql <- paste0("
    SELECT
      Count(LANGUAGE) AS N
    FROM
      TAXON
    WHERE
      LANGUAGE = '", language, "'
  ")
  if(sqlQuery(channel = channel, query = sql)$N == 0){
    sql <- "
      SELECT
        LANGUAGE AS Language
      FROM
        TAXON
      GROUP BY
        LANGUAGE
    "    
    available <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)$Language
    odbcClose(channel)
    stop(
      "No records found for language '", language, "'. Available languages are ", 
      paste0("'", available, "'", collapse = ", ")
    )
  }
  sql <- paste0("
    SELECT
      TAXON_KEY,
      ITEM_NAME,
      AUTHORITY
    FROM
      TAXON
    WHERE
      LANGUAGE = '", language, "'
  ")
  output <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)
  odbcClose(channel)
  return(output)
}
