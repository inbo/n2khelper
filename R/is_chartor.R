#' Test if the argument is either character or factor
#' @param x the object to check
#' @export
#' @importFrom assertthat 'on_failure<-'
is.chartor <- function(x){
  is.character(x) | is.factor(x)
}
on_failure(is.chartor) <- function(call, env) {
  paste0(deparse(call$x), " is neither character nor factor")
}
