#' Get the name associated with an NBN key
#' @param nbn.key A vector with NBN keys
#' @export
#' @importFrom RODBC odbcDriverConnect sqlQuery odbcClose
get_nbn_name <- function(nbn.key){
  nbn.key <- check_character(nbn.key, name = "nbn.key")
  
  channel <- odbcDriverConnect(connection = nbn.dsn)
  sql <- paste0("
    SELECT
      CASE WHEN tli.TAXON_LIST_VERSION_KEY like 'INB%' THEN 1 ELSE 0 END AS Preference,
      ns.RECOMMENDED_TAXON_VERSION_KEY AS NBNKey,
      t.LANGUAGE AS Language,
      t.ITEM_NAME AS Name, 
      ns.TAXON_TYPE AS Type,
      ns.TAXON_VERSION_STATUS AS Status
    FROM
        (
          (
              NAMESERVER AS ns 
            INNER JOIN 
              TAXON_VERSION AS tv 
            ON 
              ns.INPUT_TAXON_VERSION_KEY = tv.TAXON_VERSION_KEY
          )
        INNER JOIN
          TAXON_LIST_ITEM AS tli
        ON
          tv.TAXON_VERSION_KEY = tli.TAXON_VERSION_KEY
        )
      INNER JOIN
        TAXON AS t 
      ON 
        t.TAXON_KEY = tv.TAXON_KEY
    WHERE
      ns.RECOMMENDED_TAXON_VERSION_KEY IN (", paste0("'", unique(nbn.key), "'", collapse = ", "), ")
    GROUP BY
      CASE WHEN tli.TAXON_LIST_VERSION_KEY like 'INB%' THEN 1 ELSE 0 END,
      ns.RECOMMENDED_TAXON_VERSION_KEY,
      t.LANGUAGE,
      t.ITEM_NAME, 
      ns.TAXON_TYPE,
      ns.TAXON_VERSION_STATUS
  ")
  output <- sqlQuery(channel = channel, query = sql)
  odbcClose(channel)
 
  if(nrow(output) <= 1){
    return(output[, c("NBNKey", "Language", "Name")])
  }
  output <- ddply(
    .data = output,
    .variables = c("NBNKey", "Language"),
    function(this.key){
      if(nrow(this.key) == 1){
        return(this.key[, "Name", drop = FALSE])
      }
      preference <- which(this.key$Preference == 1) 
      if(length(preference) == 1){
        return(this.key[preference, "Name", drop = FALSE])
      }
      if(length(preference) > 1){
        this.key <- this.key[preference, ]
      }
      status.R <- which(this.key$Status == "R")
      if(length(status.R) == 1){
        return(this.key[status.R, "Name", drop = FALSE])
      }
      if(length(status.R) > 1){
        print(this.key)
        stop("yet to be written")
      }
      if(length(unique(this.key$Type)) == 1){
        combined <- paste0(
          "(", 
          paste(this.key$Name, collapse = "/"),
          ")"
        )
        warning("Multiple matching values: ", paste(this.key$Name, collapse = "/"))
        return(data.frame(Name = combined))
      }
      print(this.key)
      stop("yet to be written")
    }
  )
  return(output)
}
