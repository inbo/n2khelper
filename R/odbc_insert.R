#' Append a data.frame to a table through an ODBC connection
#' 
#' @return The status of the SQL INSERT for each row in returned but invisible.
#' @inheritParams check_id
#' @param data the data.frame
#' @export
#' @importFrom RODBC sqlQuery
odbc_insert <- function(channel, data, table){
  # check input
  if(class(channel) != "RODBC"){
    stop("channel is not an ODBC connection")
  }
  if(!is.data.frame(data)){
    stop("data must be a data.frame")
  }
  table <- check_single_character(table)
  
  # quote values when needed
  type <- sapply(data, class)
  type[type %in% c("integer", "numeric")] <- "done"
  if(any(type == "character")){
    old.fancy.quotes <- getOption("useFancyQuotes")
    options(useFancyQuotes = FALSE)
    data[, type == "character"] <- apply(
      data[, type == "character", drop = FALSE], 
      2, 
      sQuote
    )
    options(useFancyQuotes = old.fancy.quotes)
    type[type == "character"] <- "done"
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
