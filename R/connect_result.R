#' Opens an ODBC connection to the 'results' database
#' @export
#' @param username the username to connect to the database.
#' @param password the password for the username.
#' @param develop Logical value. Indicates the location of the results database
#' @importFrom assertthat assert_that is.flag noNA is.string
#' @importFrom RPostgreSQL dbConnect PostgreSQL
connect_result <- function(username, password, develop = TRUE) {
  assert_that(is.flag(develop), noNA(develop))
  assert_that(is.string(username))
  assert_that(is.string(password))

  dbname <- "n2kresult"
  assert_that(develop, msg = "Production database not yet defined")
  host <- "localhost"
  # nocov start
  dbConnect(
    drv = PostgreSQL(), host = host, dbname = dbname, user = username,
    password = password
  )
  # nocov end
}

#' Open a trusted connection to the NBN database
#' @export
#' @importFrom RODBC odbcDriverConnect
connect_nbn <- function() {
  odbcDriverConnect(connection = nbn.dsn)
}

#' connect to the unit test database
#' @inheritParams dplyr::src_postgres
#' @export
#' @importFrom RPostgreSQL dbConnect PostgreSQL
connect_ut_db <- function(
  host = "localhost",
  dbname = "n2kunittest",
  user = "unittest_analysis",
  password = "unittest",
  port = 5432,
  ...
) {
  # nocov start
  dbConnect(
    drv = PostgreSQL(), host = host, dbname = dbname, user = user,
    password = password, port = port, ...
  )
  # nocov end
}
