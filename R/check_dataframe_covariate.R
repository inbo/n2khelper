#' Check if the covariates are available in a dataframe
#' @inheritParams check_dataframe_variable
#' @param covariate The right hand side of the model as a character
#' @param response The left hand side of the model as a character
#' @importFrom assertthat assert_that is.flag is.string noNA
#' @importFrom stats as.formula
#' @export
check_dataframe_covariate <- function(
  df, covariate, response = "Count", error = TRUE
) {
  assert_that(is.string(covariate), noNA(covariate))
  assert_that(is.string(response), noNA(response))
  assert_that(is.flag(error), noNA(error))

  formula <- as.formula(paste(response, "~ ", covariate))
  check_dataframe_variable(
    df = df[1, ], variable = all.vars(formula), error = error
  )
}
