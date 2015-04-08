#' returns the path of the datasource within the git repository
#' 
#' The details are stored in the results database.
#' @param data.source.name The name of the data source
#' @inheritParams connect_result
#' @importFrom RODBC sqlQuery odbcClose odbcDriverConnect
#' @export
git_connect <- function(data.source.name, develop = TRUE){
  data.source.name <- check_single_character(data.source.name)

  channel <- connect_result(develop = develop)
  sql <- paste0("
    SELECT
      ConnectionString,
      DatasourceType.Description AS Type,
      ConnectMethod.Description AS ConnectMethod
    FROM
        (
          Datasource
        INNER JOIN
          DatasourceType
        ON
          Datasource.TypeID = DatasourceType.ID
        )
      INNER JOIN
        ConnectMethod
      ON
        Datasource.ConnectMethodID = ConnectMethod.ID
    WHERE
      Datasource.Description = '", data.source.name, "'"
  )
  connection <- sqlQuery(channel = channel, query = sql)
  odbcClose(channel)
  
  if(nrow(connection) == 0){
    stop("No connection information found for '", data.source.name, "'.")
  }
  if(nrow(connection) > 1){
    stop("Multiple lines with connection information found for '", data.source.name, "'.")
  }
  if(connection$Type != "git, tab delimited"){
    stop("'", data.source.name, "' is not a git repository but '", connection$Type, "'.")
  }
  
  return(connection$ConnectionString)
}
