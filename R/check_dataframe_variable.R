#' Check if a data.frame contains variables
#' 
#' @param df the \code{data.frame} to check
#' @param variable a vector with the names of the variable to check
#' @param name the name of the \code{data.frame} to use in the error message
#' @param error When TRUE (default), the function returns an error when a variable is missing. Otherwise it returns a warning.
#' @return The function returns TRUE when all variables are present. If returns FALSE when a variable is missing and \code{error = FALSE}.
#' @export
#' @examples
#' check_dataframe_variable(
#'  df = data.frame(a = integer(0)),
#'  variable = "a"
#' )
check_dataframe_variable <- function(df, variable, name = "df", error = TRUE){
  if(!class(df) %in% c("data.frame", "matrix")){
    stop(name, " must be a data.frame")
  }
  variable <- check_character(x = variable, name = "variable", na.action = na.fail)
  if(length(variable) == 0){
    stop("'variable' must contain at least one value")
  }
  name <- check_single_character(x = name, name = "name")
  error <- check_single_logical(x = error, name = "error")
  
  available <- variable %in% colnames(df)
  if(!all(available)){
    missing.var <- paste(variable[!available], collapse = ", ")
    if(error){
      stop("Variables missing in ", name, ": ", missing.var)
    } else {
      warning("Variables missing in ", name, ": ", missing.var)
      return(FALSE)
    }
  }
  return(TRUE)
}
