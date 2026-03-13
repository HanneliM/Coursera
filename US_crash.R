#' Read csv file
#'
#' This is a helper function that reads a CSV file from the US National Highway
#' Traffic Safety Administration's Fatality Analysis Reporting Syste
#'
#' @param filename A character string giving the path to the data file
#'
#' @return A data frame (tibble) of the data and summarises the number of fatal
#'  crashes in each US state and the date (month and year) in which the fatal
#'    injuries occured.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read()
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a FARS filename
#'
#' Generates a standardised filename for a FARS data file based on a specific year.
#'
#' @param year A numeric value or string representing the year.
#'
#' @return Character string in format of csv file
#'
#' @example
#' #' \dontrun{make_filename()
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads data for multiple years
#'
#' This function reads data files (multiple) for a vector of years and date
#'    object with month and year objects
#'
#' @param years Numeric value regarding information on year and month in which fatal injuries occured
#' @param year Numeric value or string representing the year of injuries from csv file
#'
#' @return Character string of year and month in which fatal injuries occured.
#'
#' @importFrom dplyr mutate select
#'
#' @example
#' #' \dontrun{fars_read_years()
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarises fatal injuries by year and month
#'
#' This function calculates the number of fatal crashes per month and year based
#'   on the provided list of years.
#'
#' @param years Numeric value regarding information on year in which fatal injuries occured
#'
#' @return A dataframe in tidy format with info on month and year of injuries
#'
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#'
#' @example
#' #' \dontrun{fars_summarize_years()
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Geographical location of fatal injuries in US
#'
#' Map that displays where in the US (longitude and latitude) the fatal injuries occur per year
#'
#' @param year Numeric value regarding information on year and month in which fatal injuries occured
#' @param state.num Integer that represents a US state ID
#'
#' @return A plot /map of where the fatal injuries occurred in the US states by longitude and latitude
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#'
#' @example
#' #' \dontrun{fars_map_state()
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
