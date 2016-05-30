#' Split dates into periods within each year
#'
#' The periods are defined by a day and month. The same day from different years with be in the same period.
#' @param x the dates in POSIXt or Date format
#' @param dm the breakpoints of the periods in 'day-month' format
#' @param include.last Should the last period include the last day? Defaults to TRUE
#' @export
#' @importFrom plyr ddply
#' @importFrom lubridate year is.Date is.POSIXt
#' @importFrom utils tail
#' @examples
#' x <- as.POSIXct(
#'     c("2015-01-01", "2014-01-02", "2013-01-03", "2012-01-31", "2011-02-01", "2012-12-31")
#' )
#' cut_date(x, dm = c("1-1", "1-2", "1-3"))
cut_date <- function(x, dm, include.last = TRUE){
  if (is.Date(x)) {
    x <- as.POSIXlt(x)
  }
  if (!is.POSIXt(x)) {
    stop("'x' must be POSIXt")
  }
  dm <- check_character(dm)
  wrong.format <- grep(
    "^([123456789]|[0123][0123456789])-([123456789]|0[123456789]|1[012])$",
    dm,
    invert = TRUE
  )
  if (length(wrong.format) > 0) {
    dm.wrong <- paste0("'", dm[wrong.format], "'", collapse = ", ")
    stop("'dm' requires a day-month format. Mismatching values: ", dm.wrong)
  }
  include.last <- check_single_logical(include.last)

  raw <- data.frame(
    Original = x,
    Year = year(x),
    Order = seq_along(x)
  )
  output <- ddply(raw, "Year", function(x){
    current.breaks <- as.POSIXlt(
      paste0(dm, "-", x$Year[1]),
      format = "%d-%m-%Y"
    )
    dm <- dm[order(current.breaks)]
    current.breaks <- sort(current.breaks)
    if (include.last) {
      current.breaks[length(current.breaks)] <- 24 * 3600 +
        current.breaks[length(current.breaks)]
    }
    last.bracket <- c(rep(")", length(dm) - 2), ifelse(include.last, "]", ")"))
    labels <- paste0("[", head(dm, -1), ", ", tail(dm, -1), last.bracket)
    x$Cut <- cut(
      x$Original,
      breaks = current.breaks,
      labels = labels,
      right = FALSE
    )
    x
  })
  output$Cut[order(output$Order)]
}
