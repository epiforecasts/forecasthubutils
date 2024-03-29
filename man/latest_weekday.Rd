% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{latest_weekday}
\alias{latest_weekday}
\title{Find the Latest Target Weekday}
\usage{
latest_weekday(date = Sys.Date(), day = 1, char = FALSE)
}
\arguments{
\item{date}{A date, by default the current system date.}

\item{day}{Numeric, defaults to 1 (Monday). Day of the
week to find. See ?floor_date for documentation.}

\item{char}{Logical, defaults to `TRUE`. Should the date be
returned as a character string}
}
\value{
A date or character string identifying
 the latest target day of the week
}
\description{
Find the Latest Target Weekday
}
