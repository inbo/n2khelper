#' Split dates into periods within each year
#'
#' The periods are defined by a day and month.
#' The same day from different years with be in the same period.
#' @param x the dates in POSIXt or Date format.
#' @param dm the breakpoints of the periods in 'day-month' format.
#' @param include_last Should the last period include the last day?
#' Defaults to `TRUE`.
#' @export
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom plyr ddply
#' @importFrom lubridate year is.Date is.POSIXt
#' @importFrom utils tail head
#' @examples
#' x <- as.POSIXct(
#'     c(
#'     "2015-01-01", "2014-01-02", "2013-01-03", "2012-01-31", "2011-02-01",
#'     "2012-12-31"
#'    )
#' )
#' cut_date(x, dm = c("1-1", "1-2", "1-3"))
cut_date <- function(x, dm, include_last = TRUE) {
  if (is.Date(x)) {
    x <- as.POSIXlt(x)
  }
  assert_that(inherits(x, "POSIXt"))
  dm <- check_character(dm)
  wrong_format <- grep(
    "^([123456789]|[0123][0123456789])-([123456789]|0[123456789]|1[012])$",
    dm,
    invert = TRUE
  )
  assert_that(
    length(wrong_format) == 0,
    msg = paste(
      "'dm' requires a day-month format. Mismatching values:",
      paste0("'", dm[wrong_format], "'", collapse = ", ")
    )
  )
  assert_that(is.flag(include_last), noNA(include_last))

  raw <- data.frame(
    Original = x, Year = year(x), Order = seq_along(x)
  )
  output <- ddply(raw, "Year", function(x) {
    current_breaks <- as.POSIXlt(
      paste0(dm, "-", x$Year[1]),
      format = "%d-%m-%Y"
    )
    dm <- dm[order(current_breaks)]
    current_breaks <- sort(current_breaks)
    if (include_last) {
      current_breaks[length(current_breaks)] <- 24 * 3600 +
        current_breaks[length(current_breaks)]
    }
    last_bracket <- c(rep(")", length(dm) - 2), ifelse(include_last, "]", ")"))
    labels <- paste0("[", head(dm, -1), ", ", tail(dm, -1), last_bracket)
    x$cut <- cut(
      x$Original, breaks = current_breaks, labels = labels, right = FALSE
    )
    x
  })
  output$cut[order(output$Order)]
}
