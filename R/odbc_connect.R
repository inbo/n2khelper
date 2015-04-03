#' connect to a data source through ODBC
#' 
#' The connection string is stored in the results database.
#' @param data.source The name of the data source
#' @inheritParams connect_result
#' @importFrom RODBC sqlQuery odbcClose odbcDriverConnect
odbc_connect <- function(data.source, develop = TRUE){
  data.source <- check_single_character(data.source)

  channel <- connect_result(develop = develop)
  sql <- paste0("
    SELECT
      ConnectionString,
      Username,
      Password,
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
      Datasource.Description = '", data.source, "'"
  )
  connection <- sqlQuery(channel = channel, query = sql)
  odbcClose(channel)
  
  if(nrow(connection) == 0){
    stop("No connection information found for '", data.source, "'.")
  }
  if(nrow(connection) > 1){
    stop("Multiple lines with connection information found for '", data.source, "'.")
  }
  if(connection$Type %in% "git, tab delimited"){
    stop("ODBC connection not available for '", data.source, "'. Use a connection for '", connection$Type, "'")
  }
  
  if(connection$Type == "Microsoft SQL Server"){
    connection.string <- connection$ConnectionString
    if(!is.na(connection$Username)){
      connection.string <- paste0(connection.string, "uid=", connection$Username, ";")
    }
    if(!is.na(connection$Password)){
      connection.string <- paste0(connection.string, "pwd=", connection$Password, ";")
    }
    data.channel <- odbcDriverConnect(connection.string)
    return(data.channel)
  }
  stop("odbc_connect() doesn't know how to handle '", connection$Type, "'.")
}
