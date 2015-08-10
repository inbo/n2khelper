#' Append a data.frame to a table through an ODBC connection
#' 
#' @return The status of the SQL INSERT for each row in returned but invisible.
#' @inheritParams odbc_get_id
#' @param data the data.frame
#' @param append Append the data or overwrite existing rows?
#' @param rows.at.time Number of rows to insert in one SQL statement
#' @export
#' @importFrom RODBC sqlClear sqlQuery
odbc_insert <- function(data, table, channel, schema = "dbo", append = TRUE, rows.at.time = 1000){
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  if (nrow(data) == 0) {
    return(invisible(-2))
  }
  rows.at.time <- check_single_strictly_positive_integer(
    rows.at.time, 
    name = "rows.at.time"
  )
  if (rows.at.time > 1000) {
    rows.at.time <- 1000
    warning("'rows.at.time' is limited to 1000")
  }
  check_dbtable_variable(
    table = table, 
    variable = colnames(data), 
    channel = channel,
    schema = schema
  )
  
  if (!append) {
    sqlClear(channel = channel, sqtable = paste0(schema, ".", table))
  }
  
  # quote values when needed
  type <- sapply(data, class)
  type[type %in% c("integer", "numeric")] <- "done"
  
  relevant <- which(type == "factor")
  if (length(relevant) > 0) {
    data[, relevant] <- sapply(relevant, function(i){
      levels(data[, i])[data[, i]]
    })
    type[relevant] <- "character"
  }
  
  relevant <- which(type == "character")
  if (length(relevant) > 0) {
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
  
  # Format POSIX fields to datetime
  relevant <- which(sapply(type,  identical, c("POSIXct", "POSIXt")))
  if (length(relevant) > 0) {
    data[, relevant] <- apply(
      data[, relevant, drop = FALSE], 
      2, 
      strftime,
      format = "'%Y%m%d %H:%M:%S'"
    )
    type[relevant] <- "done"
  }
  
  # Convert TRUE / FALSE to 1 / 0
  relevant <- which(sapply(type, identical, "logical"))
  if (length(relevant) > 0) {
    data[, relevant] <- 1L * data[, relevant]
    type[relevant] <- "done"
  }
  
  # test if all data types are handled
  if (any(type != "done")) {
    stop(
      "Unhandled data types: ", 
      unique(type[type != "done"])
    )
  }
  
  # Convert NA
  data[is.na(data)] <- "NULL"
  
  # prepare values
  values <- apply(data, 1, paste, collapse = ", ")
  if (rows.at.time > 1) {
    set <- seq_along(values) %/% rows.at.time
    tmp <- aggregate(values, list(set), FUN = paste, collapse = "),\n(")
    values <- tmp$x
  }
  
  # prepare columnnames
  columns <- paste(colnames(data), collapse = ", ")

  sql <- paste0(
    "INSERT INTO
      ", schema, ".", table, " (", columns, ")
    VALUES
    (", values, ")"
  )
  sql.status <- sapply(
    sql, 
    sqlQuery, 
    channel = channel, 
    errors = FALSE
  )
  if (any(sql.status == -1)) {
    if (rows.at.time == 1) {
      warning(
        "Inserting data failed on rows: ", 
        paste(unname(which(sql.status == -1)), collapse = ", ")
      )
    } else {
      warning("Inserting data failed on some rows. Try again with rows.at.time = 1 to see which rows fail.")
    }
  }
  return(invisible(sql.status))
}
