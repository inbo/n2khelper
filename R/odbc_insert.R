#' Append a data.frame to a table through an ODBC connection
#' 
#' @return The status of the SQL INSERT for each row in returned but invisible.
#' @inheritParams check_id
#' @param data the data.frame
#' @export
#' @importFrom RODBC sqlQuery
odbc_insert <- function(channel, data, table){
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  check_dbtable_variable(
    table = table, 
    variable = colnames(data), 
    channel = channel
  )
  
  # quote values when needed
  type <- sapply(data, class)
  type[type %in% c("integer", "numeric")] <- "done"
  if(any(type == "character")){
    relevant <- which(type == "character")
    data[, relevant] <- sapply(relevant, function(i){
      gsub("\\'", "\\'\\'", data[, i])
    })
    old.fancy.quotes <- getOption("useFancyQuotes")
    options(useFancyQuotes = FALSE)
    data[, relevant] <- apply(
      data[, relevant, drop = FALSE], 
      2, 
      sQuote
    )
    options(useFancyQuotes = old.fancy.quotes)
    type[relevant] <- "done"
  }
  
  relevant <- which(sapply(type,  identical, c("POSIXct", "POSIXt")))
  if(length(relevant) > 0){
    data[, relevant] <- apply(
      data[, relevant, drop = FALSE], 
      2, 
      strftime,
      format = "'%Y%m%d %H:%M:%S'"
    )
    type[relevant] <- "done"
  }
  # test if all data types are handled
  if(any(type != "done")){
    stop(
      "Unhandled data types: ", 
      unique(type[type != "done"])
    )
  }
  
  # prepare values
  values <- apply(data, 1, paste, collapse = ", ")
  
  # prepare columnnames
  columns <- paste(colnames(data), collapse = ", ")

  sql <- paste0(
    "INSERT INTO
    ", table, " (", columns, ")
    VALUES
      (", values, ")"
  )
  sql.status <- sapply(
    sql, 
    sqlQuery, 
    channel = channel, 
    errors = FALSE
  )
  if(any(sql.status == -1)){
    warning(
      "Inserting data failed on rows: ", 
      paste(unname(which(sql.status == -1)), collapse = ", ")
    )
  }
  return(invisible(sql.status))
}
