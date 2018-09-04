#' Opens an ODBC connection to the 'results' database
#' @export
#' @importFrom dplyr src_postgres
#' @param username the username to connect to the database.
#' @param password the password for the username.
#' @param develop Logical value. Indicates the location of the results database
#' @importFrom assertthat assert_that is.flag noNA is.string
connect_result <- function(username, password, develop = TRUE){
  assert_that(is.flag(develop))
  assert_that(noNA(develop))
  assert_that(is.string(username))
  assert_that(is.string(password))

  dbname <- "n2kresult"
  if (develop) {
  # nocov start
    host <- "localhost"
  # nocov end
  } else {
    stop("Production database not yet defined")
  }
  # nocov start
  src_postgres(
    host = host,
    dbname = dbname,
    user = username,
    password = password
  )
  # nocov end
}

#' Open a trusted connection to the NBN database
#' @export
connect_nbn <- function(){
  odbcDriverConnect(connection = nbn.dsn)
}

#' connect to the unit test database
#' @inheritParams dplyr::src_postgres
#' @export
#' @importFrom dplyr src_postgres
connect_ut_db <- function(
  host = "localhost",
  dbname = "n2kunittest",
  user = "unittest_analysis",
  password = "unittest",
  port = 5432,
  ...
){
  # nocov start
  src_postgres(
    host = host,
    dbname = dbname,
    user = user,
    password = password,
    ...
  )
  # nocov end
}
