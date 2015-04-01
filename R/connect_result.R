#' Opens an ODBC connection to the 'results' database
#' @export
#' @importFrom RODBC odbcDriverConnect
#' @param develop
connect_result <- function(develop = TRUE){
  if(develop){
    odbcDriverConnect("Driver=SQL Server;Server=INBODEV02\\development;Database=D0116on00_SoortenMeetnetAnalyse;Trusted_Connection=Yes;")
  } else {
    stop("Production databank not yet defined")
  }
}
