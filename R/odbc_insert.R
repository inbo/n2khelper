#' Append a data.frame to a table through an ODBC connection
#'
#' @return The status of the SQL INSERT for each row in returned but invisible.
#' @inheritParams odbc_get_id
#' @param data the data.frame
#' @param append Append the data or overwrite existing rows?
#' @param rows_at_time Number of rows to insert in one SQL statement
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr %>% data_frame funs group_by transmute
#' mutate_each_ summarise_ select_
#' @importFrom RODBC sqlClear sqlColumns sqlQuery
#' @importFrom rlang .data
#' @importFrom utils write.table
odbc_insert <- function(
  data, table, channel, schema = "dbo", append = TRUE, rows_at_time = 1000
) {
  assert_that(inherits(data, "data.frame"))
  if (nrow(data) == 0) {
    return(invisible(-2))
  }
  finite <- sapply(
    data,
    function(x) {
      any(is.finite(x))
    }
  )
  assert_that(all(finite), msg = "data contains infinite values")
  assert_that(is.count(rows_at_time))
  if (rows_at_time > 1000) {
    rows_at_time <- 1000
    warning("'rows_at_time' is limited to 1000")
  }
  check_dbtable_variable(
    table = table, variable = colnames(data), channel = channel, schema = schema
  )

  # nocov start
  if (!append) {
    sqlClear(channel = channel, sqtable = paste0(schema, ".", table))
  }
  # nocov end

  # try bulkcopy to insert data
  connection <- attr(channel, "connection.string") %>%
    strsplit(split = ";") %>%
    unlist() %>%
    strsplit(split = "=")
  names(connection) <- sapply(
    connection,
    function(x) {
      x[1]
    }
  )
  connection <- sapply(
    connection,
    function(x) {
      x[2]
    }
  )

  dbtable <- sqlColumns(channel = channel, sqtable = table, schema = schema) %>%
    select_(~COLUMN_NAME)
  data[, dbtable$COLUMN_NAME[!dbtable$COLUMN_NAME %in% colnames(data)]] <- NA
  file <- tempfile(fileext = ".txt")
  write.table(
    x = data[, dbtable$COLUMN_NAME], file = file, quote = FALSE, sep = "\t",
    row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8", na = ""
  )
  bcp <- sprintf(
    "bcp %s.%s.%s in %s -c -S %s -T",
    connection["DATABASE"], schema, table, file, connection["SERVER"]
  )
  bcp_result <- try(system(bcp, intern = TRUE))
  if (!inherits(bcp_result, "try-error")) {
    return(invisible(0))
  }
  warning("bulkcopy failed, falling back to INSERT. Failing command:\n", bcp)

  # quote values when needed
  type <- sapply(data, class)
  type[type %in% c("integer", "numeric")] <- "done"

  relevant <- which(type == "factor")
  if (length(relevant) > 0) {
    unfactor <- function(x) {
      levels(x)[x]
    }
    data <- mutate_each_(data, funs(unfactor), vars = names(relevant))
    type[relevant] <- "character"
  }


  relevant <- which(type == "character")
  if (length(relevant) > 0) {
    add_quote <- function(x) {
      sQuote(gsub("\\'", "\\'\\'", x))
    }
    old_fancy_quotes <- getOption("useFancyQuotes")
    on.exit(options(useFancyQuotes = old_fancy_quotes), add = TRUE)
    options(useFancyQuotes = FALSE)
    data <- mutate_each_(data, funs(add_quote), vars = names(relevant))
    type[relevant] <- "done"
  }

  # Format POSIX fields to datetime
  relevant <- which(sapply(type,  identical, c("POSIXct", "POSIXt")))
  if (length(relevant) > 0) {
    fmt_posix <- function(x) {
      strftime(x, format = "'%Y%m%d %H:%M:%S'")
    }
    data <- mutate_each_(data, funs(fmt_posix), vars = names(relevant))
    type[relevant] <- "done"
  }

  # Convert TRUE / FALSE to 1 / 0
  relevant <- which(sapply(type, identical, "logical"))
  if (length(relevant) > 0) {
    data <- mutate_each_(data, funs(as.integer), vars = names(relevant))
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
  values <- data_frame(
    Value = apply(data, 1, paste, collapse = ", "),
    Group = seq_len(nrow(data)) %/% rows_at_time
  ) %>%
    group_by(.data$Group) %>%
    summarise_(
      Value = ~paste(Value, collapse = "),\n(")
    ) %>%
    transmute(
      SQL = sprintf(
        "INSERT INTO %s.%s (%s) VALUES (%s)", .data$schema, .data$table,
        paste(colnames(data), collapse = ", "), .data$Values
      )
    )

  # nocov start
  sql_status <- sapply(
    values$SQL,
    sqlQuery,
    channel = channel,
    errors = FALSE
  )
  if (any(sql_status == -1)) {
    if (rows_at_time == 1) {
      warning(
        "Inserting data failed on rows: ",
        paste(unname(which(sql_status == -1)), collapse = ", ")
      )
    } else {
      warning(
        "Inserting data failed on some rows. Try again with rows_at_time = 1 to
        see which rows fail."
      )
    }
  }
  return(invisible(sql_status)) # nocov end
}
