#' connect to a data source through ODBC
#'
#' The connection string is stored in the results database.
#' @param data.source.name The name of the data source
#' @param username the username in case the ConnectMethod is "Credentials supplied by the user running the report". Ignored in all other cases.
#' @param password the password to be used in combination with the username.
#' @param channel the ODBC channel to the database with the connection strings
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom dplyr tbl %>% inner_join filter_ select_ collect
#' @importFrom tidyr spread_
#' @importFrom RODBC odbcDriverConnect
#' @export
odbc_connect <- function(data.source.name, username, password, channel){
  assert_that(is.string(data.source.name))
  check_dbtable_variable(
    table = "datasource",
    variable = c(
      "id", "datasource_type", "description"
    ),
    channel = channel
  )
  # nocov start
  check_dbtable_variable(
    table = "datasource_type",
    variable = c("id", "description"),
    channel = channel
  )
  check_dbtable_variable(
    table = "datasource_parameter",
    variable = c("id", "description"),
    channel = channel
  )
  check_dbtable_variable(
    table = "datasource_value",
    variable = c("datasource", "parameter", "value", "destroy"),
    channel = channel
  )

  connection <- tbl(channel, "datasource") %>%
    filter_(~description == data.source.name) %>%
    select_(datasource_id = ~id, datasource_type_id = ~datasource_type) %>%
    inner_join(
      tbl(channel, "datasource_type") %>%
        select_(datasource_type_id = ~id, datasource_type = ~description),
      by = "datasource_type_id"
    ) %>%
    select_(~-datasource_type_id) %>%
    inner_join(
      tbl(channel, "datasource_value") %>%
        filter_(~is.na(destroy)) %>%
        select_(~datasource, ~parameter, ~value),
      by = c("datasource_id" = "datasource")
    ) %>%
    inner_join(
      tbl(channel, "datasource_parameter") %>%
        select_(~id, ~description),
      by = c("parameter" = "id")
    ) %>%
    select_(~datasource_type, parameter = ~description, ~value) %>%
    collect() %>%
    spread_(key_col = "parameter", value_col = "value")

  if (nrow(connection) == 0) {
    stop("No connection information found for '", data.source.name, "'.")
  }
  if (nrow(connection) > 1) {
    stop(
      "Multiple lines with connection information found for '",
      data.source.name, "'."
    )
  }

  if (connection$datasource_type == "Microsoft SQL Server") {
    assert_that(has_name(connection, "server"))
    assert_that(has_name(connection, "dbname"))
    if (connection$connect_method ==
        "Credentials stored securely in the report server") {
      assert_that(has_name(connection, "username"))
      assert_that(has_name(connection, "password"))
      driver <- ifelse(
        .Platform$OS.type == "windows",
        "SQL Server",
        "FreeTDS"
      )
      connection.string <- sprintf(
        "Driver=%s;Server=%s;Database=%s;uid=%s;pwd=%s;",
        driver,
        connection$server,
        connection$dbname,
        connection$username,
        connection$password
      )
    } else {
      stop("'", connection$connect_method, "' is to do")
    }
    return(odbcDriverConnect(connection.string))
  } else {
    stop("'", connection$datasource_type, "' is to do")
  }
  return(channel)
  # nocov end
}
