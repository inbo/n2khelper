#' connect to a data source through ODBC
#'
#' The connection string is stored in the results database.
#' @param data_source_name The name of the data source
#' @param username the username in case the ConnectMethod is `"Credentials
#' supplied by the user running the report"`.
#' Ignored in all other cases.
#' @param password the password to be used in combination with the username.
#' @param channel the ODBC channel to the database with the connection strings
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom dplyr tbl %>% inner_join filter select collect
#' @importFrom rlang .data UQ
#' @importFrom tidyr pivot_wider
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @export
odbc_connect <- function(data_source_name, username, password, channel) {
  # nocov start
  assert_that(is.string(data_source_name))
  check_dbtable_variable(
    table = "datasource",
    variable = c(
      "id", "datasource_type", "description"
    ),
    channel = channel
  )
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
    filter(UQ(as.name("description")) == data_source_name) %>%
    select(
      datasource_id = .data$id,
      datasource_type_id = .data$datasource_type
    ) %>%
    inner_join(
      tbl(channel, "datasource_type") %>%
        select(
          datasource_type_id = .data$id,
          datasource_type = .data$description
        ),
      by = "datasource_type_id"
    ) %>%
    select(-.data$datasource_type_id) %>%
    inner_join(
      tbl(channel, "datasource_value") %>%
        filter(is.na(UQ(as.name("destroy")))) %>%
        select(.data$datasource, .data$parameter, .data$value),
      by = c("datasource_id" = "datasource")
    ) %>%
    inner_join(
      tbl(channel, "datasource_parameter") %>%
        select(.data$id, .data$description),
      by = c("parameter" = "id")
    ) %>%
    select(
      .data$datasource_type,
      parameter = .data$description,
      .data$value
    ) %>%
    collect() %>%
    pivot_wider(names_from = .data$parameter, values_from = .data$value)

  assert_that(
    nrow(connection) > 0,
    msg = paste0(
      "No connection information found for '", data_source_name, "'."
    )
  )
  assert_that(
    nrow(connection) == 1,
    msg = paste0(
      "Multiple lines with connection information found for '",
      data_source_name, "'."
    )
  )

  if (connection$datasource_type == "Microsoft SQL Server") {
    assert_that(has_name(connection, "server"))
    assert_that(has_name(connection, "dbname"))
    driver <- ifelse(
      .Platform$OS.type == "windows",
      "SQL Server",
      "{ODBC Driver 17 for SQL Server}"
    )
    if (connection$connect_method ==
        "Credentials stored securely in the report server") {
      assert_that(has_name(connection, "username"))
      assert_that(has_name(connection, "password"))
      connection_string <- sprintf(
        "Driver=%s;Server=%s;Database=%s;uid=%s;pwd=%s;",
        driver,
        connection$server,
        connection$dbname,
        connection$username,
        connection$password
      )
    } else if (connection$connect_method == "Trusted connection") {
      connection_string <- sprintf(
        "Driver=%s;Server=%s;Database=%s;Trusted_Connection=yes;",
        driver,
        connection$server,
        connection$dbname
      )
    } else {
      stop("'", connection$connect_method, "' is to do")
    }
  } else {
    stop("'", connection$datasource_type, "' is to do")
  }
  return(
    dbConnect(odbc(), .connection_string = connection_string)
  )
  # nocov end
}
