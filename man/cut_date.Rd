% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_date.R
\name{cut_date}
\alias{cut_date}
\title{Split dates into periods within each year}
\usage{
cut_date(x, dm, include_last = TRUE)
}
\arguments{
\item{x}{the dates in POSIXt or Date format.}

\item{dm}{the breakpoints of the periods in 'day-month' format.}

\item{include_last}{Should the last period include the last day?
Defaults to \code{TRUE}.}
}
\description{
The periods are defined by a day and month.
The same day from different years with be in the same period.
}
\examples{
x <- as.POSIXct(
    c(
    "2015-01-01", "2014-01-02", "2013-01-03", "2012-01-31", "2011-02-01",
    "2012-12-31"
   )
)
cut_date(x, dm = c("1-1", "1-2", "1-3"))
}
