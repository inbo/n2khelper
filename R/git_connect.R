#' Returns the path of the datasource within the git repository
#'
#' The details are stored in the results database.
#' @inheritParams odbc_connect
#' @inheritParams git_connection
#' @param type Use 'ssh' or 'https' for authentication
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom dplyr %>%
#' @importFrom DBI dbQuoteString dbGetQuery
#' @importFrom tidyr spread_
#' @importFrom git2r repository cred_user_pass
#' @export
git_connect <- function(
  data.source.name,
  channel,
  type = c("ssh", "https"),
  username = character(0),
  password = character(0),
  commit.user,
  commit.email
){
  type <- match.arg(type)
  assert_that(is.string(data.source.name))
  check_dbtable_variable( #nocov start
    table = "datasource",
    variable = c("id", "datasource_type", "connect_method"),
    channel = channel
  )
  check_dbtable_variable(
    table = "datasource_type",
    variable = c("id", "description"),
    channel = channel
  )
  check_dbtable_variable(
    table = "connect_method",
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
    variable = c("datasource", "parameter", "value"),
    channel = channel
  )

  connection <- sprintf(
    "SELECT
      datasource_type.description AS datasource_type,
      connect_method.description AS connect_method,
      datasource_parameter.description as parameter,
      datasource_value.value
    FROM
      (
          (
            datasource
          INNER JOIN
            datasource_type
          ON
            datasource.datasource_type = datasource_type.ID
          )
        INNER JOIN
          connect_method
        ON
          datasource.connect_method = connect_method.id
      )
    INNER JOIN
      (
        datasource_value
      INNER JOIN
        datasource_parameter
      ON
        datasource_value.parameter = datasource_parameter.id
      )
    ON
      datasource_value.datasource = datasource.id
    WHERE
      datasource.description = %s AND
      datasource_type.description = %s",
    dbQuoteString(channel, data.source.name),
    sprintf("git, tab delimited %s", type) %>%
      dbQuoteString(conn = channel)
  ) %>%
    dbGetQuery(conn = channel) %>%
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
  assert_that(has_name(connection, "path"))
  assert_that(has_name(connection, "repository"))

  if (
    connection$connect_method ==
      "Credentials stored securely in the report server"
  ) {
    if (has_name(connection, "key")) {
      username <- connection$key

    } else {
      assert_that(has_name(connection, "username"))
      username <- connection$username
    }
    assert_that(has_name(connection, "password"))
    password <- connection$password
  }

  if (type == "ssh") {
    return(
      git_connection(
        repo.path = connection$repository,
        local.path = connection$path,
        key = username,
        password = password,
        commit.user = commit.user,
        commit.email = commit.email
      )
    )
  }
  return(
    git_connection(
      repo.path = connection$repository,
      local.path = connection$path,
      username = username,
      password = password,
      commit.user = commit.user,
      commit.email = commit.email
    )
  ) #nocov end
}
