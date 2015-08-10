#' Check if the object is a single strictly positive integer
#' @param x the object to check
#' @param name the name of the object to use in the error message
#' @param tolerance was it an acceptable difference in case x is numeric
#' @return The function the strictly positive integer back. it throws an error when the input is not a strictly positive integer
#' @export
#' @examples
#' check_single_strictly_positive_integer(20)
check_single_strictly_positive_integer <- function(x, name = "x", tolerance = 1e-10){
  name <- check_single_character(x = name, name = "name")
  if(!class(x) %in% c("integer", "numeric")){
    stop(name, " must be integer")
  }
  if(length(x) != 1){
    stop(name, " must be a single number")
  }
  if(!class(tolerance) %in% c("integer", "numeric")){
    stop("tolerance must be numeric")
  }
  if(tolerance <= 0){
    stop("tolerance must be positive")
  }
  if(tolerance > 0.5){
    stop("tolerance > 0.5 is not relevant")
  }
  
  if(is.numeric(x)){
    if(abs(x - round(x)) > tolerance){
      stop(name, " is not integer")
    } else {
      x <- as.integer(x)
    }
  }
  if(x <= 0){
    stop(name, " must be strictly positive")
  }
  return(x)
}
