#' Returns the path of the datasource within the git repository
#'
#' The details are stored in the results database.
#' @inheritParams odbc_connect
#' @inheritParams git_connection
#' @param type Use 'ssh' or 'https' for authentication
#' @importFrom assertthat assert_that is.string
#' @importFrom RODBC sqlQuery odbcClose odbcDriverConnect
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
  check_dbtable_variable(
    table = "Datasource",
    variable = c(
      "ConnectionString", "Username", "Password", "TypeID", "ConnectMethodID"
    ),
    channel = channel
  )
  check_dbtable_variable( #nocov start
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
      Datasource.Description = '", data.source.name, "' AND
      DatasourceType.Description = 'git, tab delimited ", type, "'"
  )
  connection <- sqlQuery(
    channel = channel,
    query = sql,
    stringsAsFactors = FALSE
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
  if (length(grep("path='.*'", connection$ConnectionString))) {
    path <- gsub("'.*$", "", gsub("^.*path='", "", connection$ConnectionString))
  } else {
    stop("Path not defined in ", connection$ConnectionString)
  }
  if (length(grep("repo='.*'", connection$ConnectionString))) {
    repo.path <- gsub(
      "'.*$",
      "",
      gsub("^.*repo='", "", connection$ConnectionString)
    )
  } else {
    stop("Repository not defined in ", connection$ConnectionString)
  }
  if (length(grep("branch='.*'", connection$ConnectionString))) {
    branch <- gsub(
      "'.*$",
      "",
      gsub("^.*branch='", "", connection$ConnectionString)
    )
  } else {
    branch <- "master"
  }

  if (
    connection$ConnectMethod ==
      "Credentials stored securely in the report server"
  ) {
    username <- connection$Username
    password <- connection$Password
  }

  if (type == "ssh") {
    return(
      git_connection(
        repo.path = repo.path,
        local.path = path,
        key = username,
        password = password,
        commit.user = commit.user,
        commit.email = commit.email
      )
    )
  }
  return(
    git_connection(
      repo.path = repo.path,
      local.path = path,
      username = username,
      password = password,
      commit.user = commit.user,
      commit.email = commit.email
    )
  ) #nocov end
}
