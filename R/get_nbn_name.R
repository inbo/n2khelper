#' Get the name associated with an NBN key
#' @param nbn.key A vector with NBN keys
#' @inheritParams get_nbn_key
#' @export
#' @importFrom RODBC odbcDriverConnect sqlQuery odbcClose
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% ungroup group_by_ select_ mutate_ semi_join filter_ summarise_ count_
#' @importFrom tidyr spread_
get_nbn_name <- function(nbn.key, channel){
  nbn.key <- check_character(nbn.key, name = "nbn.key")
  assert_that(inherits(channel, "RODBC"))

  output <- sprintf("
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
      CASE WHEN tli.TAXON_LIST_VERSION_KEY like 'INB%%' THEN 'Yes' ELSE 'No' END,
      ns.RECOMMENDED_TAXON_VERSION_KEY,
      t.LANGUAGE,
      t.ITEM_NAME,
      ns.TAXON_TYPE,
      ns.TAXON_VERSION_STATUS",
    paste0("'", unique(nbn.key), "'", collapse = ", ")
  ) %>%
    sqlQuery(
      channel = channel,
      stringsAsFactors = FALSE,
      as.is = TRUE
    )

  output <- output %>%
    count_(c("NBNKey", "Language", "Preference")) %>%
    spread_(key_col = "Preference", value_col = "n", fill = 0L) %>%
    mutate_(Preference = ~ifelse(Yes > 0, "Yes", "No")) %>%
    semi_join(x = output, by = c("NBNKey", "Language", "Preference"))
  output <- output %>%
    count_(c("NBNKey", "Language", "Status")) %>%
    spread_(key_col = "Status", value_col = "n", fill = 0L) %>%
    mutate_(Status = ~ifelse(R > 0, "R", "S")) %>%
    semi_join(x = output, by = c("NBNKey", "Language", "Status")) %>%
    group_by_(~NBNKey, ~Language) %>%
    summarise_(
      Name = ~paste(Name, collapse = "/"),
      Multi = ~n() > 1
    ) %>%
    ungroup()

  if (any(output$Multi)) {
    text <- output %>%
      filter_(~Multi) %>%
      summarise_(
        Name = ~paste(Name, collapse = "\n")
      )
    warning("Multiple matching values:\n", text$Name)
  }

  return(output) # nocov end
}
