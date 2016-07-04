#' Opens an ODBC connection to the 'results' database
#' @export
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom DBI dbConnect
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
  drv <- RPostgreSQL::PostgreSQL()
  if (develop) {
    host <- "localhost"
  } else {
    stop("Production database not yet defined")
  }
  dbConnect(
    drv = drv,
    host = host,
    dbname = dbname,
    user = username,
    password = password
  )
}
