% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odbc_get_multi_id.R
\name{odbc_get_multi_id}
\alias{odbc_get_multi_id}
\title{Get the corresponding id's}
\usage{
odbc_get_multi_id(
  data,
  id_field,
  merge_field,
  table,
  channel,
  create = FALSE,
  select = TRUE,
  rows_at_time = 1000
)
}
\arguments{
\item{data}{the data.frame}

\item{id_field}{the id fields}

\item{merge_field}{the merge fields}

\item{table}{The name of the table}

\item{channel}{the open dplyr connection to the database.}

\item{create}{When \code{TRUE}, the function creates unmatching records AND
updates attributes.
Defaults to \code{FALSE}.}

\item{select}{Return the matching ID's when \code{TRUE}.
Returns invisible \code{NULL} when \code{FALSE}.
select = \code{FALSE} is only relevant in combination with create = \code{TRUE}.}

\item{rows_at_time}{Number of rows to insert in one SQL statement}
}
\value{
a data.frame with data and the id's
}
\description{
Get the corresponding id's
}
