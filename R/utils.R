#' @title Dates to Epiweek
#'
#' @description
#' MISSING#'
#'
#' @param df A data.frame
#' @return A data.frame
#' @importFrom lubridate epiweek
#' @importFrom dplyr filter group_by count left_join mutate
#' @importFrom tibble tibble
#' @export

dates_to_epiweek <- function(df){

  seq <- tibble::tibble(date = unique(df$date),
                        epiweek = lubridate::epiweek(as.Date(date)),
                        day = weekdays(as.Date(date)))

  epiweek_end_date <- seq %>%
    dplyr::filter(day == "Saturday")

  epiweek_complete <- seq %>%
    dplyr::group_by(epiweek) %>%
    dplyr::count() %>%
    dplyr::filter(n == 7) %>%
    dplyr::left_join(epiweek_end_date, by = "epiweek")

  df_dated <- df %>%
    dplyr::mutate(epiweek = lubridate::epiweek(as.Date(date)),
                  epiweek_end = date %in% epiweek_end_date$date,
                  epiweek_full = epiweek %in% epiweek_complete$epiweek)

  return(df_dated)
}
