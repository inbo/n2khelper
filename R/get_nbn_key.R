#' Get the NBN key of a species
#' @param name a vector of species names to check
#' @param language The language to use. Defaults to "la" 'scientific name"
#' @param channel An open RODBC channel to the NBN database
#' @param authority Do the species names include authority?
#' @export
#' @importFrom RODBC odbcDriverConnect sqlQuery odbcClose
#' @importFrom dplyr %>% distinct group_by filter ungroup select slice arrange
#' @importFrom rlang .data
#' @importFrom assertthat assert_that is.string
#' @importFrom DBI dbQuoteString dbGetQuery
get_nbn_key <- function(name, language = "la", channel, authority = FALSE){
  # nocov start
  assert_that(
    is.character(name),
    is.string(language),
    inherits(channel, "DBIConnection")
  )
  sprintf("
    SELECT Count(LANGUAGE) AS N
    FROM TAXON
    WHERE LANGUAGE = %s",
    dbQuoteString(channel, language)
  ) %>%
    dbGetQuery(conn = channel) -> counts

  if (counts$N == 0) {
    available <- dbGetQuery(
      channel, "
      SELECT LANGUAGE AS Language
      FROM TAXON
      GROUP BY LANGUAGE"
    )$Language
    stop(
      "No records found for language '", language,
      "'. Available languages are ",
      paste0("'", available, "'", collapse = ", ")
    )
  }
  if (isTRUE(authority)) {
    sql <- sprintf("
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
        t.LANGUAGE = %s AND
        t.ITEM_NAME + ' ' + t.AUTHORITY IN (%s)",
      dbQuoteString(channel, language),
      paste0(dbQuoteString(channel, name), collapse = ", ")
    )
  } else {
    sql <- sprintf("
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
        t.LANGUAGE = %s AND
        t.ITEM_NAME IN (%s)",
      dbQuoteString(channel, language),
      paste0(dbQuoteString(channel, name), collapse = ", ")
    )
  }
  dbGetQuery(channel, sql) %>%
    distinct() %>%
    group_by(.data$InputName) %>%
    filter(.data$PreferenceInput == max(.data$PreferenceInput)) %>%
    filter(.data$PreferenceOutput == max(.data$PreferenceOutput)) %>%
    arrange(.data$NBNKey) %>%
    slice(1) %>%
    ungroup() %>%
    select(-"PreferenceInput", -"PreferenceOutput") %>%
    as.data.frame()
  #nocov end
}
