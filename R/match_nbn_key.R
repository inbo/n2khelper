#' Merge NBN keys into a species dataframe
#' @param species a data.frame with the species names
#' @param nbn.key a data.frame with the NBN keys
#' @param variable the name of the variable of species to match with InputName from nbn.key
#' @export
match_nbn_key <- function(species, nbn.key, variable){
  variable <- check_single_character(x = variable, name = "variable")
  check_dataframe_variable(
    df = species,
    variable = variable,
    name = "species"
  )
  check_dataframe_variable(
    df = nbn.key,
    variable = c("InputName", "NBNKey"),
    name = "nbn.key"
  )
  if(any(table(nbn.key$InputName) > 1)){
    stop("Duplicate 'InputName' in 'nbn.key'")
  }
  if("NBNKey" %in% colnames(species)){
    stop("NBNKey present")
  }
  
  input <- data.frame(
    Original = check_character(species[, variable])
  )
  input$LowerCase <- tolower(input$Original)
  nbn.key$LowerCase <- tolower(nbn.key$InputName)
  input <- merge(input, nbn.key[, c("LowerCase", "NBNKey")], all.x = TRUE)
  merge(species, input[, c("Original", "NBNKey")], by.x = variable, by.y = "Original")
}
