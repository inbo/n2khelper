#' Merge NBN keys into a species dataframe
#' @param species a data.frame with the species names
#' @param nbn_key a data.frame with the NBN keys
#' @param variable the name of the variable of species to match with InputName
#'    from nbn_key
#' @importFrom assertthat assert_that is.string noNA
#' @export
match_nbn_key <- function(species, nbn_key, variable) {
  assert_that(is.string(variable), noNA(variable))
  check_dataframe_variable(df = species, variable = variable, name = "species")
  check_dataframe_variable(
    df = nbn_key, variable = c("InputName", "NBNKey"),
    name = "nbn_key"
  )
  if (any(table(nbn_key$InputName) > 1)) {
    stop("Duplicate 'InputName' in 'nbn_key'")
  }
  if ("NBNKey" %in% colnames(species)) {
    stop("NBNKey present")
  }

  input <- data.frame(
    Original = check_character(species[, variable])
  )
  input$lowercase <- tolower(input$Original)
  nbn_key$lowercase <- tolower(nbn_key$InputName)
  input <- merge(input, nbn_key[, c("lowercase", "NBNKey")], all.x = TRUE)
  merge(
    species,
    input[, c("Original", "NBNKey")],
    by.x = variable,
    by.y = "Original"
  )
}
