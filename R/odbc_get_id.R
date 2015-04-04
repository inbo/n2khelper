#' Get the corresponding id's
#' 
#' @param data the data.frame
#' @param id.field the id fields
#' @param merge.field the merge fields
#' @param table the table name
#' @param channel the open ODBC channel
#' @param create Should the function create new rows when no corresponding rows are found? Defaults to FALSE.
#' @export
#' @return a data.frame with data and the id's
#' @importFrom digest digest
#' @importFrom RODBC sqlSave sqlQuery
odbc_get_id <- function(data, id.field, merge.field, table, channel, create = FALSE){
  check_dataframe_variable(df = data, variable = merge.field, name = "data")
  table <- check_single_character(table, name = "table")
  if(class(channel) != "RODBC"){
    stop("channel is not an ODBC connection")
  }
  create <- check_single_logical(create)
  
  tmptable <- digest(data, algo = "sha1")
  sqlSave(
    channel = channel, dat = data, tablename = tmptable, append = FALSE, rownames = TRUE
  )
  
  id.field.table <- paste0(table, ".", id.field)
  merge.field.table <- paste0("tmp.", merge.field)
  selected.field <- paste(
    c(id.field.table, merge.field.table), 
    collapse = ",\n      "
  )
  join.on <- paste0(
    "tmp.", merge.field, " = ", table, ".", merge.field, 
    collapse = " AND\n"
  )
  
  if(create){
    null.field <- paste(
      id.field.table, "IS NULL", 
      collapse = " AND\n"
    )
    sql <- paste0("
      SELECT
        ", paste(merge.field.table, collapse = ",\n      "), "
      FROM
        ", tmptable, " AS tmp
      LEFT JOIN
        ", table, "
      ON
        ", join.on, "
      WHERE
        ", null.field
    )
    unknown <- sqlQuery(channel = channel, query = sql)
    to.append <- merge(data, unknown)
    odbc_insert(channel = channel, data = to.append, table = table)
  }
  
  sql <- paste0("
    SELECT
      ", selected.field, "
    FROM
      ", tmptable, " AS tmp
    INNER JOIN
      ", table, "
    ON
      ", join.on
  )
  id <- sqlQuery(channel = channel, query = sql)
  sql <- paste("DROP TABLE", tmptable)
  sqlQuery(channel = channel, query = sql)
  return(id)
}
