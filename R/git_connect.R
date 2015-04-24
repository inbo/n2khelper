#' Returns the path of the datasource within the git repository
#' 
#' The details are stored in the results database.
#' @inheritParams odbc_connect
#' @importFrom RODBC sqlQuery odbcClose odbcDriverConnect
#' @importFrom git2r repository cred_user_pass
#' @export
git_connect <- function(
  data.source.name, 
  channel, 
  username = character(0), 
  password = character(0)
){
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
  if(length(grep("path='.*'", connection$ConnectionString))){
    path <- gsub("'.*$", "", gsub("^.*path='", "", connection$ConnectionString))
  } else {
    stop("Path not defined in ", connection$ConnectionString)
  }
  if(length(grep("repo='.*'", connection$ConnectionString))){
    repo.path <- gsub("'.*$", "", gsub("^.*repo='", "", connection$ConnectionString))
  } else {
    stop("Repository not defined in ", connection$ConnectionString)
  }
  if(length(grep("branch='.*'", connection$ConnectionString))){
    branch <- gsub("'.*$", "", gsub("^.*branch='", "", connection$ConnectionString))
  } else {
    branch <- "master"
  }
  
  if(connection$ConnectMethod == "Credentials stored securely in the report server"){
    username <- connection$Username 
    password <- connection$Password
  }
  
  output <- git_connection(
    repo.path = repo.path, local.path = path, username = username, password = password
  )
  return(output)
}
