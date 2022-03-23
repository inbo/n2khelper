#' Returns the path of the datasource within the git repository
#'
#' The details are stored in the results database.
#' @inheritParams odbc_connect
#' @inheritParams git_connection
#' @param type Use 'ssh' or 'https' for authentication
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom dplyr %>% collect filter inner_join select tbl
#' @importFrom git2r repository cred_user_pass
#' @importFrom rlang .data
#' @importFrom tidyr spread_
#' @export
git_connect <- function(
  data_source_name, channel, type = c("ssh", "https"), username = character(0),
  password = character(0), commit_user, commit_email
) {
  type <- match.arg(type)
  assert_that(is.string(data_source_name))
  # nocov start
  check_dbtable_variable(
    table = "datasource",
    variable = c("id", "datasource_type"),
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
    variable = c("datasource", "parameter", "value"),
    channel = channel
  )

  type <- sprintf("git, tab delimited %s", type)
  connection <- tbl(channel, "datasource") %>%
    filter(.data$description == data_source_name) %>%
    inner_join(
      tbl(channel, "datasource_type"),
      by = c("datasource_type" = "id")
    ) %>%
    filter(.data$description.y == type) %>%
    select(datasource = .data$id, datasource_type = .data$description.y) %>%
    inner_join(
      tbl(channel, "datasource_value") %>%
        inner_join(
          tbl(channel, "datasource_parameter"),
          by = c("parameter" = "id")
        ) %>%
        select(.data$datasource, parameter = .data$description, .data$value),
      by = "datasource"
    ) %>%
    collect() %>%
    spread_(key_col = "parameter", value_col = "value")
  if (nrow(connection) == 0) {
    stop("No connection information found for '", data_source_name, "'.")
  }
  if (nrow(connection) > 1) {
    stop(
      "Multiple lines with connection information found for '",
      data_source_name, "'."
    )
  }
  assert_that(has_name(connection, "connect_method"))
  assert_that(has_name(connection, "repo"))

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
  } else {
    stop(connection$connect_method, "not yet defined.")
  }

  if (type == "ssh") {
    return(
      git_connection(
        repo_path = connection$repo, key = username, password = password,
        commit_user = commit_user, commit_email = commit_email
      )
    )
  }
  return(
    git_connection(
      repo_path = connection$repo, username = username, password = password,
      commit_user = commit_user, commit_email = commit_email
    )
  ) #nocov end
}
