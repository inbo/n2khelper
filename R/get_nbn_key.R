#' Get the NBN key of a species
#' @param name a vector of species names to check
#' @param language The language to use. Defaults to "la" 'scientific name"
#' @inheritParams connect_result
#' @export
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom plyr ddply
get_nbn_key <- function(name, language = "la", develop = TRUE){
  name <- check_character(name, name = "name")
  language <- check_single_character(language, name = "language")
  
  channel <- odbc_connect(data.source.name = "NBN data", develop = develop)
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
      tr.ITEM_NAME AS GenericName,
      CASE WHEN tlir.TAXON_LIST_VERSION_KEY like 'INB%' THEN 1 ELSE 0 END AS Preference
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
          (
            TAXON AS tr
          INNER JOIN
            TAXON_VERSION AS tvr
          ON
            tr.TAXON_KEY = tvr.TAXON_KEY
          )
        INNER JOIN
          TAXON_LIST_ITEM AS tlir
        ON
          tvr.TAXON_VERSION_KEY = tlir.TAXON_VERSION_KEY
        )
      ON
        ns.RECOMMENDED_TAXON_VERSION_KEY = tvr.TAXON_VERSION_KEY
    WHERE
      t.LANGUAGE = '", language, "' AND
      t.ITEM_NAME IN (", paste0("'", name, "'", collapse = ", "), ")
    GROUP BY
      t.ITEM_NAME,
      ns.RECOMMENDED_TAXON_VERSION_KEY,
      tr.ITEM_NAME,
      CASE WHEN tlir.TAXON_LIST_VERSION_KEY like 'INB%' THEN 1 ELSE 0 END
  ")
  output <- sqlQuery(channel = channel, query = sql, stringsAsFactors = FALSE)
  odbcClose(channel)
  if(nrow(output) <= 1){
    output$Preference <- NULL
    return(output)
  }
  if(all(table(output$InputName) == 1)){
    output$Preference <- NULL
    return(output)
  }
  preferred <- output[output$Preference == 1, c("InputName", "NBNID", "GenericName")]
  non.preferred <- output[output$Preference == 0, c("InputName", "NBNID", "GenericName")]
  non.preferred <- non.preferred[!non.preferred$InputName %in% preferred$InputName, ]
  output <- rbind(preferred, non.preferred)
  if(any(table(output$InputName) > 1)){
    warning("Duplicate matching keys")
  }
  return(output)
}
