#' Opens an ODBC connection to the 'results' database
#' @export
#' @importFrom RODBC odbcDriverConnect
#' @param develop Logical value. Indicates the location ton the results database
#' @param username the username to connect to the database. Use a trusted connection when missing
#' @param password the password for the username. Ignored when using a trusted connection.
#' @importFrom assertthat assert_that is.flag noNA is.string
connect_result <- function(develop = TRUE, username, password){
  assert_that(is.flag(develop))
  assert_that(noNA(develop))
  if (develop) {
    server <- "INBODEV02\\development"
  } else {
    stop("Production database not yet defined")
  }
  driver <- ifelse(.Platform$OS.type == "windows", "SQL Server", "FreeTDS")
  if (missing(username)) {
    authentication <- "Trusted_Connection=Yes;"
  } else {
    assert_that(is.string(username))
    if (username == "") {
      authentication <- "Trusted_Connection=Yes;"
    } else {
      assert_that(is.string(password))
      authentication <- sprintf("Uid=%s;Pwd=%s", username, password)
    }
  }
  connection <- sprintf(
    "Driver=%s;Server=%s;Database=D0116on00_SoortenMeetnetAnalyse;%s",
    driver,
    server,
    authentication
  )
  odbcDriverConnect(connection)
}
