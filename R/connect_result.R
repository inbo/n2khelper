#' Opens an ODBC connection to the 'results' database
#' @export
#' @importFrom RODBC odbcDriverConnect
#' @param develop Logical value. Indicates the location ton the results database
connect_result <- function(develop = TRUE){
  develop <- check_single_logical(develop)
  if(develop){
    odbcDriverConnect("Driver=SQL Server;Server=INBODEV02\\development;Database=D0116on00_SoortenMeetnetAnalyse;Trusted_Connection=Yes;")
  } else {
    stop("Production databank not yet defined")
  }
}
