#' Check if the covariates are available in a dataframe
#' @inheritParams check_dataframe_variable
#' @param covariate The right hand side of the model as a character
#' @param response The left hand side of the model as a character
#' @export
check_dataframe_covariate <- function(
  df,
  covariate,
  response = "Count",
  error = TRUE
){
  covariate <- check_single_character(covariate, name = "covariate")
  response <- check_single_character(response, name = "response")
  error <- check_single_logical(error)

  formula <- as.formula(paste(response, "~ ", covariate))
  output <- check_dataframe_variable(
    df = df[1, ],
    variable = all.vars(formula),
    error = error
  )
  return(output)
}
