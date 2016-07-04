#' Get the id of the matching records
#' @export
#' @param table The name of the table
#' @param variable A vector with the names of the columns
#' @param value the content of the variable
#' @param schema The schema of the table. Defaults to public
#' @param channel the open dplyr connection to the database.
#' @importFrom lazyeval interp
#' @importFrom dplyr %>% filter_ select_ collect
odbc_get_id <- function(table, variable, value, schema = "public", channel){
  value <- check_character(value, name = "value")
  if (length(value) == 0) {
    stop("at least one value is needed")
  }
  check_dbtable_variable(table = table, variable = variable, channel = channel)
  # nocov start
  if (length(value) != length(variable)) {
    stop("the number of values doesn't match the number of variables")
  }

  dots <- sapply(
    seq_along(variable),
    function(i){
      interp(~x == y, x = as.name(variable[i]), y = value[i])
    }
  )
  tbl(channel, table) %>%
    filter_(.dots = dots) %>%
    select_(~id) %>%
    collect() %>%
    unlist() # nocov end
}
