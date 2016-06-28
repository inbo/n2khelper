#' Get the corresponding id's
#'
#' @param data the data.frame
#' @param id.field the id fields
#' @param merge.field the merge fields
#' @param create When TRUE, the function creates unmatching records AND updates attributes. Defaults to FALSE.
#' @param select Return the matching ID's when TRUE. Returns invisible NULL when FALSE. select = FALSE is only relevant in combination with create = TRUE.
#' @inheritParams check_dbtable_variable
#' @inheritParams odbc_insert
#' @export
#' @return a data.frame with data and the id's
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom RODBC sqlQuery odbcClose
odbc_get_multi_id <- function(
  data,
  id.field,
  merge.field,
  table,
  channel,
  create = FALSE,
  select = TRUE,
  rows.at.time = 1000
){
  assert_that(is.flag(create))
  assert_that(is.flag(select))
  assert_that(noNA(create))
  assert_that(noNA(select))
  if (! create && ! select) {
    stop("The combination of select = FALSE and create = FALSE is meaningless")
  }

  check_dataframe_variable(df = data, variable = merge.field, name = "data")
  check_dbtable_variable(
    table = table,
    variable = c(id.field, merge.field),
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
    rows.at.time = rows.at.time
  )

  join.on <- paste0(
    "(TARGET.", merge.field, " = SOURCE.", merge.field, " OR (TARGET.",
    merge.field, " IS NULL AND SOURCE.", merge.field, " IS NULL))",
    collapse = " AND\n        "
  )

  if (create) {
    attribute.field <- colnames(data)[!colnames(data) %in% merge.field]
    if (length(attribute.field) == 0) {
      update.command <- ""
    } else {
      update.clause <- paste0(
        "TARGET.", attribute.field, " <> SOURCE.", attribute.field,
        " OR TARGET.", attribute.field, " IS NULL OR SOURCE.", attribute.field,
        " IS NULL",
        collapse = " OR "
      )
      update.set <- paste0(
        "TARGET.", attribute.field, " = SOURCE.", attribute.field,
        collapse = ",\n"
      )
      update.command <- paste0("
      WHEN MATCHED AND (", update.clause, ") THEN
        UPDATE SET
          ", update.set
      )
    }
    sql <- paste0("
      MERGE
        dbo.", table, " AS TARGET
      USING
        staging.", table, " AS SOURCE
      ON
        ", join.on, update.command, "
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
      true.error <- grep(
        paste0("^[RODBC] ERROR: Could not SQLExecDirect '", sql),
        output
      )
      if (length(true.error) > 0) {
        warning(output)
      }
    }
  }

  if (! select) {
    return(invisible(NULL))
  }

  # select matching id's
  selected.field <- paste0(
    "TARGET.",
    c(id.field, merge.field),
    collapse = ",\n      "
  )
  sql <- paste0("
    SELECT
      ", selected.field, "
    FROM
      staging.", table, " AS SOURCE
    INNER JOIN
      dbo.", table, " AS TARGET
    ON
      ", join.on
  )
  id <- sqlQuery(
    channel = channel,
    query = sql,
    stringsAsFactors = FALSE,
    as.is = TRUE
  )
  return(id)
}
