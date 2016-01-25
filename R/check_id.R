#' Test if an id exists in a given field of the table
#' @inheritParams odbc_get_id
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom RODBC sqlQuery
check_id <- function(value, variable, table, channel){
  assert_that(is.count(value))
  variable <- check_single_character(variable, name = "variable")
  check_dbtable_variable(table = table, variable = variable, channel = channel)

  # nocov start
  sql <- paste("SELECT", variable, "FROM", table, "WHERE", variable, "=", value)
  selection <- sqlQuery(
    channel = channel,
    query = sql,
    stringsAsFactors = FALSE,
    as.is = TRUE
  )
  if (!inherits(selection, "data.frame")) {
    if (length(grep("Invalid column name", selection)) > 0) {
      stop(
        "The variable '", variable, "' doesn't exists in table '", table, "'"
      )
    }
    if (length(grep("Conversion failed", selection))) {
      stop(paste(selection, collapse = "\n"))
    }
  }

  if (nrow(selection) == 0) {
    return(FALSE)
  } else {
    if (nrow(selection) > 1) {
      warning(
        "The id '", value, "' in variable '", variable, "' of table '", table,
        "' exists in multiple rows"
      )
    }
    return(TRUE)
  }
  # nocov end
}
