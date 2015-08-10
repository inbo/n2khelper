#' Check if the object is a single POSIX
#' 
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @param past Should the function throw an error when x is in the future? Default is FALSE.
#' @return The function gives the single POSIX back. It throws an error when the input is not a single character.
#' @export
#' @examples
#' check_single_posix(Sys.time())
check_single_posix <- function(x, name = "x", past = FALSE){
  if(!any(class(x) %in% c("POSIXct", "POSIXlt", "POSIXt"))){
    stop(name, " must be a POSIXct or POSIXlt object")
  }
  if(length(x) != 1){
    stop(name, " must be a single POSIXct or POSIXlt object")
  }
  if(past && x > Sys.time()){
    stop(name, " is in the future.")
  }
  return(x)
}
