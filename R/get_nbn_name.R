#' Get the name associated with an NBN key
#' @param nbn_key A vector with NBN keys
#' @inheritParams get_nbn_key
#' @export
#' @importFrom assertthat assert_that
#' @importFrom DBI dbQuoteString dbGetQuery
#' @importFrom dplyr %>% count mutate semi_join group_by summarise ungroup n
#' filter
#' @importFrom rlang .data
#' @importFrom tidyr spread
get_nbn_name <- function(nbn_key, channel) {
  # nocov start
  assert_that(
    is.character(nbn_key),
    inherits(channel, "DBIConnection")
  )

  dbQuoteString(channel, unique(nbn_key)) %>%
    paste0(collapse = ", ") %>%
    sprintf(fmt = "
      SELECT
        CASE
          WHEN tli.TAXON_LIST_VERSION_KEY like 'INB%%'
          THEN 'Yes' ELSE 'No' END AS Preference,
        ns.RECOMMENDED_TAXON_VERSION_KEY AS NBNKey,
        t.LANGUAGE AS Language,
        t.ITEM_NAME AS Name,
        ns.TAXON_TYPE AS Type,
        ns.TAXON_VERSION_STATUS AS Status
      FROM
          (
            (
                NAMESERVER AS ns
              INNER JOIN
                TAXON_VERSION AS tv
              ON
                ns.INPUT_TAXON_VERSION_KEY = tv.TAXON_VERSION_KEY
            )
          INNER JOIN
            TAXON_LIST_ITEM AS tli
          ON
            tv.TAXON_VERSION_KEY = tli.TAXON_VERSION_KEY
          )
        INNER JOIN
          TAXON AS t
        ON
          t.TAXON_KEY = tv.TAXON_KEY
      WHERE
        ns.RECOMMENDED_TAXON_VERSION_KEY IN (%s)
      GROUP BY
        CASE
          WHEN tli.TAXON_LIST_VERSION_KEY like 'INB%%'
          THEN 'Yes'
          ELSE 'No'
        END,
        ns.RECOMMENDED_TAXON_VERSION_KEY,
        t.LANGUAGE,
        t.ITEM_NAME,
        ns.TAXON_TYPE,
        ns.TAXON_VERSION_STATUS"
    ) %>%
    dbGetQuery(conn = channel) -> output

  if (length(unique(output$Preference)) > 1) {
    output %>%
      count(.data$NBNKey, .data$Language, .data$Preference) %>%
      spread(key = "Preference", value = "n", fill = 0L) %>%
      mutate(Preference = ifelse(.data$Yes > 0, "Yes", "No")) %>%
      semi_join(x = output, by = c("NBNKey", "Language", "Preference")) ->
      output
  }
  if (length(unique(output$Status)) > 1) {
    output %>%
      count(.data$NBNKey, .data$Language, .data$Status) %>%
      spread(key = "Status", value = "n", fill = 0L) %>%
      mutate(Status = ifelse(.data$R > 0, "R", "S")) %>%
      semi_join(x = output, by = c("NBNKey", "Language", "Status")) -> output
  }
  output %>%
    group_by(.data$NBNKey, .data$Language) %>%
    summarise(
      Name = paste(.data$Name, collapse = "/"),
      Multi = n() > 1
    ) %>%
    ungroup() -> output

  if (any(output$Multi)) {
    text <- output %>%
      filter(.data$Multi) %>%
      summarise(Name = paste(.data$Name, collapse = "\n"))
    warning("Multiple matching values:\n", text$Name)
  }

  return(output) # nocov end
}
