#' Check if the object is a character
#' 
#' Factors are converted to character.
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @param na.action \code{link[stats]{na.fail}} throws an error in case of \code{NA} (default). \code{link[stats]{na.omit}} will return \code{x} without the \code{NA} values. \code{link[stats]{na.pass}} will return \code{x} with the \code{NA} values.
#' @return The function gives the character back. it throws an error when the input is not a character.
#' @export
#' @examples
#' check_character(c("20", "b"))
check_character <- function(x, name = "x", na.action = na.fail){
  if(!class(x) %in% c("character", "factor")){
    stop(name, " must be character")
  }
  x <- na.action(x)
  if(is.factor(x)){
    return(levels(x)[x])
  } else {
    return(x)
  }
}
