#' connect to a data source through ODBC
#'
#' The connection string is stored in the results database.
#' @param data.source.name The name of the data source
#' @param username the username in case the ConnectMethod is "Credentials supplied by the user running the report". Ignored in all other cases.
#' @param password the password to be used in combination with the username.
#' @param channel the ODBC channel to the database with the connection strings
#' @importFrom RODBC sqlQuery odbcClose odbcDriverConnect
#' @export
odbc_connect <- function(data.source.name, username, password, channel){
  data.source.name <- check_single_character(data.source.name)
  check_dbtable_variable(
    table = "Datasource",
    variable = c(
      "ConnectionString", "Username", "Password", "TypeID", "ConnectMethodID"
    ),
    channel = channel
  )
  # nocov start
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
  connection <- sqlQuery(
    channel = channel,
    query = sql,
    stringsAsFactors = FALSE,
    as.is = TRUE
  )

  if (nrow(connection) == 0) {
    stop("No connection information found for '", data.source.name, "'.")
  }
  if (nrow(connection) > 1) {
    stop(
      "Multiple lines with connection information found for '",
      data.source.name, "'."
    )
  }
  if (connection$Type %in% "git, tab delimited") {
    stop(
      "ODBC connection not available for '", data.source.name,
      "'. Use a connection for '", connection$Type, "'"
    )
  }

  if (connection$Type == "Microsoft SQL Server") {
    connection.string <- connection$ConnectionString
    if (connection$ConnectMethod == "Windows integrated security") {
      connection.string <- paste0(connection.string, "Trusted_Connection=True;")
    }
    if (connection$ConnectMethod ==
        "Credentials stored securely in the report server"
    ) {
      if (!is.na(connection$Username)) {
        connection.string <- paste0(
          connection.string, "uid=", connection$Username, ";"
        )
      }
      if (!is.na(connection$Password)) {
        connection.string <- paste0(
          connection.string, "pwd=", connection$Password, ";"
        )
      }
    }
    if (connection$ConnectMethod ==
        "Credentials supplied by the user running the report"
    ) {
      username <- check_single_character(username, name = "username")
      password <- check_single_character(password, name = "password")
      connection.string <- paste0(connection.string, "uid=", username, ";")
      connection.string <- paste0(connection.string, "pwd=", password, ";")
    }
    data.channel <- odbcDriverConnect(connection.string)
    return(data.channel)
  }
  stop("odbc_connect() doesn't know how to handle '", connection$Type, "'.")
  # nocov end
}
