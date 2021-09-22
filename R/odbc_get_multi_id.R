#' Get the corresponding id's
#'
#' @param data the data.frame
#' @param id_field the id fields
#' @param merge_field the merge fields
#' @param create When `TRUE`, the function creates unmatching records AND
#' updates attributes.
#' Defaults to `FALSE`.
#' @param select Return the matching ID's when `TRUE`.
#' Returns invisible `NULL` when `FALSE`.
#' select = `FALSE` is only relevant in combination with create = `TRUE`.
#' @inheritParams check_dbtable_variable
#' @inheritParams odbc_insert
#' @export
#' @return a data.frame with data and the id's
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom RODBC sqlQuery odbcClose
odbc_get_multi_id <- function(
  data, id_field, merge_field, table, channel, create = FALSE, select = TRUE,
  rows_at_time = 1000
) {
  assert_that(is.flag(create), noNA(create))
  assert_that(is.flag(select), noNA(select))
  assert_that(
    create | select,
    msg = "The combination of select = FALSE and create = FALSE is meaningless"
  )

  check_dataframe_variable(df = data, variable = merge_field, name = "data")
  check_dbtable_variable(
    table = table,
    variable = c(id_field, merge_field),
    schema = "dbo",
    channel = channel
  )

  # empty staging table and fill it
  odbc_insert(
    channel = channel,
    data = data,
    table = table,
    schema = "staging",
    append = FALSE,
    rows_at_time = rows_at_time
  )

  join_on <- paste0(
    "(TARGET.", merge_field, " = SOURCE.", merge_field, " OR (TARGET.",
    merge_field, " IS NULL AND SOURCE.", merge_field, " IS NULL))",
    collapse = " AND\n        "
  )

  if (create) {
    attribute_field <- colnames(data)[!colnames(data) %in% merge_field]
    if (length(attribute_field) == 0) {
      update_command <- ""
    } else {
      update_clause <- paste0(
        "TARGET.", attribute_field, " <> SOURCE.", attribute_field,
        " OR TARGET.", attribute_field, " IS NULL OR SOURCE.", attribute_field,
        " IS NULL",
        collapse = " OR "
      )
      update_set <- paste0(
        "TARGET.", attribute_field, " = SOURCE.", attribute_field,
        collapse = ",\n"
      )
      update_command <- paste0("
      WHEN MATCHED AND (", update_clause, ") THEN
        UPDATE SET
          ", update_set
      )
    }
    sql <- paste0("
      MERGE
        dbo.", table, " AS TARGET
      USING
        staging.", table, " AS SOURCE
      ON
        ", join_on, update_command, "
      WHEN NOT MATCHED BY TARGET THEN
        INSERT (", paste0(colnames(data), collapse = ", "), ")
        VALUES (", paste0("SOURCE.", colnames(data), collapse = ", "), ")
      ;
    ")
    output <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE,
      as.is = TRUE
    )
    if (length(output) > 0) {
      true_error <- grep(
        paste0("^[RODBC] ERROR: Could not SQLExecDirect '", sql),
        output
      )
      if (length(true_error) > 0) {
        warning(output)
      }
    }
  }

  if (!select) {
    return(invisible(NULL))
  }

  # select matching id's
  selected_field <- paste0(
    "TARGET.",
    c(id_field, merge_field),
    collapse = ",\n      "
  )
  sql <- paste0("
    SELECT
      ", selected_field, "
    FROM
      staging.", table, " AS SOURCE
    INNER JOIN
      dbo.", table, " AS TARGET
    ON
      ", join_on
  )
  id <- sqlQuery(
    channel = channel,
    query = sql,
    stringsAsFactors = FALSE,
    as.is = TRUE
  )
  return(id)
}
