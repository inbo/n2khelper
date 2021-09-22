#' Get the id of the matching records
#' @export
#' @param table The name of the table
#' @param variable A vector with the names of the columns
#' @param value the content of the variable
#' @param schema The schema of the table. Defaults to public
#' @param channel the open dplyr connection to the database.
#' @param id_variable name of the id variable
#' @importFrom lazyeval interp
#' @importFrom dplyr %>% filter_ select_ collect
odbc_get_id <- function(
  table, variable, value, schema = "public", channel, id_variable = "id"
) {
  value <- check_character(value)
  assert_that(length(value) > 0)
  check_dbtable_variable(table = table, variable = variable, channel = channel)
  assert_that(
    length(value) == length(variable),
    msg = "the number of values doesn't match the number of variables"
  )
  # nocov start

  dots <- sapply(
    seq_along(variable),
    function(i) {
      interp(~x == y, x = as.name(variable[i]), y = value[i])
    }
  )
  tbl(channel, table) %>%
    filter_(.dots = dots) %>%
    select_(id_variable) %>%
    collect() %>%
    unlist() %>%
    unname() # nocov end
}
