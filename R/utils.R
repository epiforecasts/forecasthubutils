#' Check if a Directory Exists and Create if Not
#'
#' @param dir Character string path to a directory.
#' @return NULL
#' @export
check_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  return(invisible(NULL))
}

#' Make Incidence data weekly
#'
#' @param data A data frame containing a column called 'date' and one called 'value'
#' @param weekstart integer that indicates the start of the week. Default is 7 (Sunday)
#' @param group_by A character vector with variables to group by
#' @param exclude_incomplete exclude incomplete epiweeks (not equal to 7
#' observations for that week). Default is TRUE. If an empty data.table is returned,
#' make sure that you got the grouping in `group_by` right.
#' @return A data frame grouped by week.
#' @export
#' @import data.table
#' @importFrom grates as_yrwk
#' @examples
#' df <- data.frame(value = 1:30,
#'                  date = seq.Date(from = as.Date("2020-12-20"),
#'                  to = as.Date("2021-01-18"), by = "days"),
#'                  location = "GM",
#'                  location_name = "Germany")
#'
#' computed <- make_weekly(df, group_by = "location")
make_weekly <- function(data,
                        weekstart = 7L,
                        group_by = "location",
                        exclude_incomplete = TRUE) {

  inc <- as.data.table(data)[
    , year_week := as_yrwk(date, firstday = weekstart)
  ][
    , .(value = sum(value, na.rm = TRUE),
        n = .N,
        target_end_date = max(as.Date(date))),
    by = c(group_by, "year_week")
  ]

  if (exclude_incomplete) {
    inc <- inc[n == 7]
  }

  inc[, `:=` (n = NULL, year_week = NULL)]
  return(inc)
}



#' Find the Latest Target Weekday
#'
#' @param date A date, by default the current system date.
#' @param day Numeric, defaults to 1 (Monday). Day of the
#'  week to find. See ?floor_date for documentation.
#' @param char Logical, defaults to `TRUE`. Should the date be
#'  returned as a character string
#' @return A date or character string identifying
#'  the latest target day of the week
#' @export
#' @importFrom lubridate floor_date
latest_weekday <- function(date = Sys.Date(), day = 1, char = FALSE){
  weekday <- floor_date(date, unit = "week", day)
  if (char) {
    weekday <- as.character(weekday)
  }
  return(weekday)
}

globalVariables(
  c(".", "value", "df", "year_week", "n")
)
