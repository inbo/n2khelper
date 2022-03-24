#' Try multiple languages to get a matching NBN key
#' @param species A data.frame with the name of species in one or more languages
#' @param orders the order in which the languages are tried to get a matching
#' NBN key.
#' @inheritParams get_nbn_key
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr select slice mutate
#' @export
get_nbn_key_multi <- function(species, orders = c("la", "nl", "en"), channel) {
  orders <- match.arg(orders, several.ok = TRUE)
  lang_name <- c(
    la = "ScientificName", nl = "DutchName", en = "EnglishName",
    fr = "FrenchName", de = "GermanName"
  )
  assert_that(
    inherits(species, "data.frame"),
    lang_name[orders] %in% colnames(species)
  )
  if (has_name(species, "NBNKey")) {
    warning("Existing NBNKey will be overwritten.")
    species <- select(species, -"NBNKey")
  }

  to_do <- species
  slice(species, 0) %>%
    mutate(NBNKey = character(0)) -> done

  # nocov start
  for (language in orders) {
    nbn_key <- get_nbn_key(
      name = to_do[, lang_name[language]],
      language = language,
      channel = channel
    )
    to_do <- match_nbn_key(
      species = to_do,
      nbn_key = nbn_key,
      variable = lang_name[language]
    )
    done <- rbind(done, to_do[!is.na(to_do$NBNKey), ])
    to_do <- to_do[is.na(to_do$NBNKey), ]
    if (nrow(to_do) == 0) {
      break
    }
    to_do$NBNKey <- NULL # nolint
  }

  if (nrow(to_do) > 0) {
    warning("No matches found for some records")
    to_do$NBNKey <- NA # nolint
    done <- rbind(done, to_do)
  }
  return(done) #nocov end
}
