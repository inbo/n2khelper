#' Get the NBN key of a species
#' @param name a vector of species names to check
#' @param language The language to use. Defaults to "la" 'scientific name"
#' @param channel An open RODBC channel to the NBN database
#' @param authority Do the species names include authority?
#' @export
#' @importFrom RODBC odbcDriverConnect sqlQuery odbcClose
#' @importFrom dplyr %>%
#' @importFrom assertthat assert_that is.string
get_nbn_key <- function(name, language = "la", channel, authority = FALSE){
  # nocov start
  name <- check_character(name, name = "name")
  assert_that(is.string(language))
  assert_that(inherits(channel, "RODBC"))

  sql <- sprintf("
    SELECT
      Count(LANGUAGE) AS N
    FROM
      TAXON
    WHERE
      LANGUAGE = '%s'",
    language
  )
  if (
    sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE,
      as.is = TRUE
    )$N == 0
  ) {
    sql <- "
      SELECT
        LANGUAGE AS Language
      FROM
        TAXON
      GROUP BY
        LANGUAGE
    "
    available <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )$Language
    stop(
      "No records found for language '", language,
      "'. Available languages are ",
      paste0("'", available, "'", collapse = ", ")
    )
  }
  if (authority == FALSE) {
  output <- sprintf("
    SELECT
      t.ITEM_NAME AS InputName,
      ns.RECOMMENDED_TAXON_VERSION_KEY as NBNKey,
      tr.ITEM_NAME AS GenericName,
      CASE
        WHEN tli.TAXON_LIST_VERSION_KEY like 'INB%%'
        THEN 1 ELSE 0 END AS PreferenceInput,
      CASE
        WHEN tlir.TAXON_LIST_VERSION_KEY like 'INB%%'
        THEN 1 ELSE 0 END AS PreferenceOutput,
      tv.COMMENT AS Comment,
      tv.Attribute
    FROM
      (
        (
          (
            (
              dbo.TAXON AS t
            INNER JOIN
              dbo.TAXON_VERSION AS tv
            ON
              t.TAXON_KEY = tv.TAXON_KEY
            )
          INNER JOIN
            dbo.TAXON_LIST_ITEM AS tli
          ON
            tv.TAXON_VERSION_KEY = tli.TAXON_VERSION_KEY
          )
        INNER JOIN
          (
            (
              dbo.NAMESERVER AS ns
            INNER JOIN
              dbo.TAXON_VERSION AS tvr
            ON
              ns.RECOMMENDED_TAXON_VERSION_KEY = tvr.TAXON_VERSION_KEY
            )
          INNER JOIN
            dbo.TAXON AS tr
          ON
            tr.TAXON_KEY = tvr.TAXON_KEY
          )
        ON
          tv.TAXON_VERSION_KEY = ns.INPUT_TAXON_VERSION_KEY
        )
      )
    INNER JOIN
      dbo.TAXON_LIST_ITEM AS tlir
    ON
      tlir.TAXON_LIST_ITEM_KEY = ns.RECOMMENDED_TAXON_LIST_ITEM_KEY
    WHERE
      t.LANGUAGE = '%s' AND
      t.ITEM_NAME IN (%s)",
    language,
    paste0("'", name, "'", collapse = ", ")
  ) %>%
    sqlQuery(
      channel = channel,
      stringsAsFactors = FALSE,
      as.is = TRUE
    ) %>%
    unique()

  if (anyDuplicated(output$InputName) > 1) {
    output <- output %>%
      group_by_(~InputName) %>%
      filter_(~PreferenceInput == max(PreferenceInput)) %>%
      filter_(~PreferenceOutput == max(PreferenceOutput)) %>%
      ungroup() %>%
      select_(~-PreferenceInput, ~-PreferenceOutput) %>%
      as.data.frame()
  }
  if (anyDuplicated(output$InputName) > 1) {
    warning("Duplicate matching keys")
  }
  return(output) #nocov end
} else {
    output <- sprintf("
    SELECT
      t.ITEM_NAME + ' ' + t.AUTHORITY AS InputName,
      ns.RECOMMENDED_TAXON_VERSION_KEY as NBNKey,
      tr.ITEM_NAME AS GenericName,
      CASE
        WHEN tli.TAXON_LIST_VERSION_KEY like 'INB%%'
        THEN 1 ELSE 0 END AS PreferenceInput,
      CASE
        WHEN tlir.TAXON_LIST_VERSION_KEY like 'INB%%'
        THEN 1 ELSE 0 END AS PreferenceOutput,
      tv.COMMENT AS Comment,
      tv.Attribute
    FROM
      (
        (
          (
            (
              dbo.TAXON AS t
            INNER JOIN
              dbo.TAXON_VERSION AS tv
            ON
              t.TAXON_KEY = tv.TAXON_KEY
            )
          INNER JOIN
            dbo.TAXON_LIST_ITEM AS tli
          ON
            tv.TAXON_VERSION_KEY = tli.TAXON_VERSION_KEY
          )
        INNER JOIN
          (
            (
              dbo.NAMESERVER AS ns
            INNER JOIN
              dbo.TAXON_VERSION AS tvr
            ON
              ns.RECOMMENDED_TAXON_VERSION_KEY = tvr.TAXON_VERSION_KEY
            )
          INNER JOIN
            dbo.TAXON AS tr
          ON
            tr.TAXON_KEY = tvr.TAXON_KEY
          )
        ON
          tv.TAXON_VERSION_KEY = ns.INPUT_TAXON_VERSION_KEY
        )
      )
    INNER JOIN
      dbo.TAXON_LIST_ITEM AS tlir
    ON
      tlir.TAXON_LIST_ITEM_KEY = ns.RECOMMENDED_TAXON_LIST_ITEM_KEY
    WHERE
      t.LANGUAGE = '%s' AND
      t.ITEM_NAME + ' ' + t.AUTHORITY IN (%s)",
    language,
    paste0("'", name, "'", collapse = ", ")
  ) %>%
    sqlQuery(
      channel = channel,
      stringsAsFactors = FALSE,
      as.is = TRUE
    ) %>%
    unique()

  if (anyDuplicated(output$InputName) > 1) {
    output <- output %>%
      group_by_(~InputName) %>%
      filter_(~PreferenceInput == max(PreferenceInput)) %>%
      filter_(~PreferenceOutput == max(PreferenceOutput)) %>%
      ungroup() %>%
      select_(~-PreferenceInput, ~-PreferenceOutput) %>%
      as.data.frame()
  }
  if (anyDuplicated(output$InputName) > 1) {
    warning("Duplicate matching keys")
  }
  return(output) #nocov end
}
}
