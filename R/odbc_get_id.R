#' Get the id of the matching records
#' @export
#' @param table The name of the table
#' @param ... arguments passed to `filter()`.
#' @param schema The schema of the table. Defaults to public
#' @param channel the open dplyr connection to the database.
#' @param id_variable name of the id variable
#' @importFrom dplyr %>% filter select_ collect
odbc_get_id <- function(
    table, ..., schema = "public", channel, id_variable = "id"
) {
  # nocov start
  tbl(channel, table) %>%
    filter(...) %>%
    select_(id_variable) %>%
    collect() %>%
    unlist() %>%
    unname() # nocov end
}
