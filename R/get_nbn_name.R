#' Get the name associated with an NBN key
#' @param nbn.key A vector with NBN keys
#' @inheritParams connect_result
#' @export
#' @importFrom RODBC sqlQuery odbcClose
get_nbn_name <- function(nbn.key, develop = TRUE){
  nbn.key <- check_character(nbn.key, name = "nbn.key")
  
  channel <- odbc_connect("NBN data", develop = develop)
  sql <- paste0("
    SELECT
      ns.RECOMMENDED_TAXON_VERSION_KEY AS NBNID,
      t.LANGUAGE AS Language,
      t.ITEM_NAME AS Name, 
      ns.TAXON_TYPE AS Type,
      ns.TAXON_VERSION_STATUS AS Status
    FROM
        (
            NAMESERVER AS ns 
          INNER JOIN 
            TAXON_VERSION AS tv 
          ON 
            ns.INPUT_TAXON_VERSION_KEY = tv.TAXON_VERSION_KEY
        )
      INNER JOIN
        TAXON AS t 
      ON 
        t.TAXON_KEY = tv.TAXON_KEY
    WHERE
      ns.RECOMMENDED_TAXON_VERSION_KEY IN (", paste0("'", unique(nbn.key), "'", collapse = ", "), ")
    GROUP BY
      ns.RECOMMENDED_TAXON_VERSION_KEY,
      t.LANGUAGE,
      t.ITEM_NAME, 
      ns.TAXON_TYPE,
      ns.TAXON_VERSION_STATUS
  ")
  output <- sqlQuery(channel = channel, query = sql)
  odbcClose(channel)
 
  if(nrow(output) <= 1){
    return(output[, c("NBNID", "Language", "Name")])
  }
  output <- ddply(
    .data = output,
    .variables = c("NBNID", "Language"),
    function(this.key){
      if(nrow(this.key) == 1){
        return(this.key[, "Name", drop = FALSE])
      }
      status.R <- which(this.key$Status == "R")
      if(length(status.R) == 1){
        return(this.key[status.R, "Name", drop = FALSE])
      }
      stop("yet to be written")
    }
  )
  return(output)
}
