#' Get the NBN key of a species
#' @param name a vector of species names to check
#' @param language The language to use. Defaults to "la" 'scientific name"
#' @inheritParams connect_result
#' @export
#' @importFrom RODBC sqlQuery odbcClose
get_nbn_key <- function(name, language = "la", develop = TRUE){
  name <- check_character(name, name = "name")
  language <- check_single_character(language, name = "language")
  
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
      t.ITEM_NAME AS InputName,
      ns.RECOMMENDED_TAXON_VERSION_KEY as NBNID,
      tr.ITEM_NAME AS GenericName
    FROM
        (
          (
            TAXON AS t
          INNER JOIN
            TAXON_VERSION AS tv
          ON
            t.TAXON_KEY = tv.TAXON_KEY
          )
        INNER JOIN
          NAMESERVER AS ns
        ON
          tv.TAXON_VERSION_KEY = ns.INPUT_TAXON_VERSION_KEY
        )
      INNER JOIN
        (
          TAXON AS tr
        INNER JOIN
          TAXON_VERSION AS tvr
        ON
          tr.TAXON_KEY = tvr.TAXON_KEY
        )
      ON
        ns.RECOMMENDED_TAXON_VERSION_KEY = tvr.TAXON_VERSION_KEY
    WHERE
      t.LANGUAGE = '", language, "' AND
      t.ITEM_NAME IN (", paste0("'", name, "'", collapse = ", "), ")
    GROUP BY
      t.ITEM_NAME,
      ns.RECOMMENDED_TAXON_VERSION_KEY,
      tr.ITEM_NAME
  ")
  output <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)
  odbcClose(channel)
  return(output)
}
