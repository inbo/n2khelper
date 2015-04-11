#' Get the corresponding id's
#' 
#' @param data the data.frame
#' @param id.field the id fields
#' @param merge.field the merge fields
#' @param table the table name
#' @param channel the open ODBC channel
#' @param create When TRUE, the function creates unmatching records AND updates attributes. Defaults to FALSE.
#' @export
#' @return a data.frame with data and the id's
#' @importFrom digest digest
#' @importFrom RODBC sqlClear sqlQuery
odbc_get_multi_id <- function(data, id.field, merge.field, table, channel, create = FALSE){
  check_dataframe_variable(df = data, variable = merge.field, name = "data")
  if(class(channel) != "RODBC"){
    stop("channel is not an ODBC connection")
  }
  check_dbtable_variable(
    table = paste0("dbo.", table), 
    variable = c(id.field, merge.field), 
    channel = channel
  )
  create <- check_single_logical(create)
  
  # empty staging table and fill it
  sqlClear(channel = channel, sqtable = paste0("staging.", table))
  odbc_insert(channel = channel, data = data, table = paste0("staging.", table))

  join.on <- paste0(
    "TARGET.", merge.field, " = SOURCE.", merge.field, 
    collapse = " AND\n        "
  )
  
  if(create){
    attribute.field <- colnames(data)[!colnames(data) %in% merge.field]
    if(length(attribute.field) == 0){
      update.command <- ""
    } else {
      update.clause <- paste0(
        "TARGET.", attribute.field, " <> SOURCE.", attribute.field, 
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
    output <- sqlQuery(channel = channel, query = sql)
    if(length(output) > 0){
      true.error <- grep(
        paste0("^[RODBC] ERROR: Could not SQLExecDirect '", sql), 
        output
      )
      if(length(true.error) > 0){
        warning(output)
      }
    }
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
  id <- sqlQuery(channel = channel, query = sql)
  return(id)
}
