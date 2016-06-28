#' Try multiple languages to get a matching NBN key
#' @param species A data.frame with the name of species in one or more languages
#' @param orders the order in which the languages are tried to get a matching NBN key
#' @importFrom stats aggregate
#' @export
get_nbn_key_multi <- function(species, orders = c("la", "nl", "en")){
  orders <- match.arg(orders, several.ok = TRUE)
  lang.name <- c(
    la = "ScientificName", nl = "DutchName", en = "EnglishName",
    fr = "FrenchName", de = "GermanName"
  )
  check_dataframe_variable(
    df = species,
    variable = lang.name[orders],
    name = "species"
  )

  if ("NBNKey" %in% colnames(species)) {
    warning("Existing NBNKey will be overwritten.")
    species$NBNKey <- NULL
  }

  to.do <- species
  done <- species[integer(0), ]
  done$NBNKey <- character(0)

  for (language in orders) {
    # nocov start
    nbn.key <- get_nbn_key(
      name = to.do[, lang.name[language]],
      language = language
    )
    if (max(table(nbn.key$InputName)) > 1) {
      nbn.key$INBO <- FALSE
      nbn.key$INBO[grep("^INB", nbn.key$NBNKey)] <- TRUE
      max.key <- aggregate(
        nbn.key[, "INBO", drop = FALSE],
        nbn.key[, "InputName", drop = FALSE],
        FUN = max
      )
      nbn.key <- merge(nbn.key, max.key)
    }
    if (max(table(nbn.key$InputName)) > 1) {
      stop("Duplicate matching keys for ", lang.name[language])
    }
    to.do <- match_nbn_key(
      species = to.do,
      nbn.key = nbn.key,
      variable = lang.name[language]
    )
    done <- rbind(done, to.do[!is.na(to.do$NBNKey), ])
    to.do <- to.do[is.na(to.do$NBNKey), ]
    if (nrow(to.do) == 0) {
      break
    }
    to.do$NBNKey <- NULL # nocov end
  }
  # nocov start
  if (nrow(to.do) > 0) {
    warning("No matches found for some records")
    to.do$NBNKey <- NA
    done <- rbind(done, to.do)
  }
  return(done) #nocov end
}
