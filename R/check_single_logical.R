#' Check if the object is a single character
#' 
#' Factors are converted to character.
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @return The function gives the single character back. it throws an error when the input is not a signle character.
#' @export
#' @examples
#' check_single_logical(TRUE)
check_single_logical <- function(x, name = "x"){
  if(class(x) != "logical"){
    stop(name, " must be logical")
  }
  if(length(x) != 1){
    stop(name, " must be a single logical")
  }
  if(is.na(x)){
    stop(name, " must be TRUE or FALSE")
  }
  return(x)
}
