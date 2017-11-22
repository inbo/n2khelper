#' Returns the path of the datasource within the git repository
#'
#' The details are stored in the results database.
#' @inheritParams odbc_connect
#' @inheritParams git_connection
#' @param type Use 'ssh' or 'https' for authentication
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom dplyr %>% tbl inner_join select_ filter_ collect
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
    filter_(~description == data.source.name) %>%
    inner_join(
      tbl(channel, "datasource_type"),
      by = c("datasource_type" = "id")
    ) %>%
    filter_(~description.y == type) %>%
    select_(
      datasource = ~ id,
      datasource_type = ~description.y
    ) %>%
    inner_join(
      tbl(channel, "datasource_value") %>%
        inner_join(
          tbl(channel, "datasource_parameter"),
          by = c("parameter" = "id")
        ) %>%
        select_(
          ~ datasource,
          parameter = ~description,
          ~value
        ),
      by = "datasource"
    ) %>%
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
  assert_that(has_name(connection, "connect_method"))
  assert_that(has_name(connection, "path"))
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
        repo.path = connection$repo,
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
      repo.path = connection$repo,
      local.path = connection$path,
      username = username,
      password = password,
      commit.user = commit.user,
      commit.email = commit.email
    )
  ) #nocov end
}
