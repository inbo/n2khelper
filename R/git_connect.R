#' Returns the path of the datasource within the git repository
#' 
#' The details are stored in the results database.
#' @inheritParams odbc_connect
#' @importFrom RODBC sqlQuery odbcClose odbcDriverConnect
#' @export
git_connect <- function(data.source.name, channel = channel){
  data.source.name <- check_single_character(data.source.name)
  check_dbtable_variable(
    table = "Datasource", 
    variable = c("ConnectionString", "Username", "Password", "TypeID", "ConnectMethodID"), 
    channel = channel
  )
  check_dbtable_variable(
    table = "DatasourceType", 
    variable = c("ID", "Description"), 
    channel = channel
  )
  check_dbtable_variable(
    table = "ConnectMethod", 
    variable = c("ID", "Description"), 
    channel = channel
  )
  
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
  
  if(nrow(connection) == 0){
    stop("No connection information found for '", data.source.name, "'.")
  }
  if(nrow(connection) > 1){
    stop("Multiple lines with connection information found for '", data.source.name, "'.")
  }
  if(connection$Type != "git, tab delimited"){
    stop("'", data.source.name, "' is not a git repository but '", connection$Type, "'.")
  }
  path <- gsub("'.*$", "", gsub("^path='", "", connection$ConnectionString))
  repo <- gsub("'.*$", "", gsub("^.*repo='", "", connection$ConnectionString))
  
  return(c(Path = path, Repo = repo))
}
