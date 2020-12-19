#' @title Load Truth Data
#'
#' @description
#' MISSING
#'
#'
#' @param countries character vector with the country names to download
#' @param save_dir character, directory in which to save the data
#' @param create_dir logical, whether or not to create the directory if it
#' doesn't exist yet. Default is TRUE
#' @return A data.frame
#' @importFrom readr read_csv
#' @importFrom data.table rbindlist fwrite
#' @importFrom here here
#' @export


download_truth_data <- function(countries = c("Germany", "Poland"),
                          save_dir = "data",
                          create_dir = TRUE) {

  incident_cases <- incident_deaths <- cumulative_cases <- cumulative_deaths <- list()

  check_download <- function(country, type, inc_or_cum, source) {
    data <- readr::read_csv(source)

    if (nrow(data) == 0) {
      msg <- paste("something went wrong (empty data) when downloading data for",
                   inc_or_cum, type, "in", country)
      warning(msg)
    }
    return(data)
  }

  if (length(countries) == 0) {
    stop("please provide at least one county. Currently supported are Germany, Poland and the US")
  }

  # country is US
  if ("US" %in% countries) {
    incident_cases[["US"]] <- check_download("US", "incident", "cases",
                                             "https://github.com/reichlab/covid19-forecast-hub/blob/master/data-truth/truth-Incident%20Cases.csv?raw=true")
    incident_deaths[["US"]] <- check_download("US", "incident", "deaths",
                                              "https://github.com/reichlab/covid19-forecast-hub/blob/master/data-truth/truth-Incident%20Deaths.csv?raw=true")

    cumulative_cases[["US"]] <- check_download("US", "cumulative", "cases",
                                               "https://github.com/reichlab/covid19-forecast-hub/blob/master/data-truth/truth-Cumulative%20Cases.csv?raw=true")
    cumulative_deaths[["US"]] <- check_download("US", "cumulative", "deaths",
                                                "https://github.com/reichlab/covid19-forecast-hub/blob/master/data-truth/truth-Cumulative%20Deaths.csv?raw=true")
  }

  if ("Germany" %in% countries) {
    incident_cases[["Germany"]] <- check_download("Germany", "incident", "cases",
                                             "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Incident%20Cases_Germany.csv")
    incident_deaths[["Germany"]] <- check_download("Germany", "incident", "deaths",
                                              "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Incident%20Deaths_Germany.csv")

    cumulative_cases[["Germany"]] <- check_download("Germany", "cumulative", "cases",
                                               "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Cumulative%20Cases_Germany.csv")
    cumulative_deaths[["Germany"]] <- check_download("Germany", "cumulative", "deaths",
                                                "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Cumulative%20Deaths_Germany.csv")
  }

  if ("Poland" %in% countries) {
    incident_cases[["Poland"]] <- check_download("Poland", "incident", "cases",
                                                  "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/MZ/truth_MZ-Incident%20Cases_Poland.csv")
    incident_deaths[["Poland"]] <- check_download("Poland", "incident", "deaths",
                                                   "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/MZ/truth_MZ-Incident%20Deaths_Poland.csv")

    cumulative_cases[["Poland"]] <- check_download("Poland", "cumulative", "cases",
                                                    "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/MZ/truth_MZ-Cumulative%20Cases_Poland.csv")
    cumulative_deaths[["Poland"]] <- check_download("Poland", "cumulative", "deaths",
                                                     "https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/MZ/truth_MZ-Cumulative%20Deaths_Poland.csv")
  }

  incident_cases <- data.table::rbindlist(incident_cases, use.names = TRUE)
  incident_deaths <- data.table::rbindlist(incident_deaths, use.names = TRUE)

  cumulative_cases <- data.table::rbindlist(cumulative_cases, use.names = TRUE)
  cumulative_deaths <- data.table::rbindlist(cumulative_deaths, use.names = TRUE)

  if (!dir.exists(save_dir)) {
    if (create_dir) {
      dir.create(save_dir)
    }
  }

  purrr::map(countries, .f = function(country) {
    # write incident cases and deaths
    data.table::fwrite(incident_cases, here::here(save_dir, paste0("daily-incidence-cases-", country, ".csv")))
    data.table::fwrite(incident_deaths, here::here(save_dir, paste0("daily-incidence-deaths-", country, ".csv")))

    data.table::fwrite(cumulative_cases, here::here(save_dir, paste0("daily-cumulative-cases-", country, ".csv")))
    data.table::fwrite(cumulative_deaths, here::here(save_dir, paste0("daily-cumulative-deaths-", country, ".csv")))
  })
  return(invisible(NULL))
}








#' @title Load Truth Data
#'
#' @description
#' MISSING
#'
#'
#' @param countries character vector with the country names to load
#' @param root_dir charcter. Where to look for the data to load or where to
#' download it to if \code{load_from_server = TRUE}
#' @param load_from_server logical, whether or not to download the data from
#' the server first. Default is FALSE
#' @param cases logical, whether or not to create return cases (the default)
#' or deaths (if \code{cases = FALSE})
#' @param weekly logical, whether or not to return weekly data or daily data
#' (if \code{weekly = FALSE})
#' @param national_only logical, whether or not to only return national
#' estimates (TRUE) or all estimates (if \code{national_only = FALSE})
#' @param cumulative logical, whether or not to return incident data (the
#' default) or cumulative data (if cumulative = TRUE)
#' @return A data.frame
#' @importFrom magrittr `%>%`
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom data.table fread
#' @importFrom here here
#' @export


load_truth_data <- function(countries = c("Germany", "Poland"),
                            root_dir = "data",
                            load_from_server = FALSE,
                            cases = TRUE,
                            weekly = TRUE,
                            national_only = TRUE,
                            cumulative = FALSE) {

  filter_national <- function(data) {
    if (national_only) {
      data <- dplyr::filter(data, location %in% c("GM", "PL", "US"))
    }
    return(data)
  }

  if (load_from_server) {
    download_truth_data(countries = countries, save_dir = root_dir)
  }

  incident_cases <- incident_deaths <- cumulative_cases <- cumulative_deaths <- list()

  purrr::map(countries, .f = function(country) {
    incident_cases[[country]] <<- data.table::fread(here::here(root_dir, paste0("daily-incidence-cases-", country, ".csv")))
    incident_deaths[[country]] <<- data.table::fread(here::here(root_dir, paste0("daily-incidence-deaths-", country, ".csv")))

    cumulative_cases[[country]] <- data.table::fread(here::here(root_dir, paste0("daily-cumulative-cases-", country, ".csv")))
    cumulative_deaths[[country]] <<- data.table::fread(here::here(root_dir, paste0("daily-cumulative-deaths-", country, ".csv")))

    return(invisible(NULL))
  })

  incident_cases <- data.table::rbindlist(incident_cases, use.names = TRUE)
  incident_deaths <- data.table::rbindlist(incident_deaths, use.names = TRUE)

  cumulative_cases <- data.table::rbindlist(cumulative_cases, use.names = TRUE)
  cumulative_deaths <- data.table::rbindlist(cumulative_deaths, use.names = TRUE)


  if (weekly) {
    # cases
    if (cases) {
      incident_cases_weekly <- incident_cases %>%
        dates_to_epiweek() %>%
        dplyr::filter(epiweek_full == TRUE) %>%
        dplyr::group_by(location, location_name, epiweek) %>%
        dplyr::summarise(value = sum(value),
                         target_end_date = max(date))

      if (cumulative) {
        cumulative_cases_weekly <- incident_cases_weekly %>%
          dplyr::mutate(value = cumsum(value))
        return(filter_national(cumulative_cases_weekly))
      } else {
        return(filter_national(incident_cases_weekly))
      }

      # deaths
    } else {
      incident_deaths_weekly <- incident_deaths %>%
        dates_to_epiweek() %>%
        dplyr::filter(epiweek_full == TRUE) %>%
        dplyr::group_by(location, location_name, epiweek) %>%
        dplyr::summarise(value = sum(value),
                         target_end_date = max(date))
      if (cumulative) {
        cumulative_deaths_weekly <- incident_deaths_weekly %>%
          dplyr::mutate(value = cumsum(value))
        return(filter_national(cumulative_deaths_weekly))
      } else {
        return(filter_national(incident_deaths_weekly))
      }
    }
  }

  # if not weekly
  if (cases) {
    if (cumulative) {
      return(filter_national(cumulative_cases))
    } else {
      return(filter_national(incident_cases))
    }
  } else {
    if (cumulative) {
      return(filter_national(cumulative_deaths))
    } else {
      return(filter_national(incident_deaths))
    }
  }
}

