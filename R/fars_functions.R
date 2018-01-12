#' Read files from Fatality Analysis Reporting System
#'
#' The function \code{fars_read} reads csv files from the US National Highway
#' Traffic Safety Administration's Fatality Analysis Reporting System and
#' creates a data frame out of the values.3
#'
#' @param filename  A path to a csv file. Files ending in .gz, .bz2, .xz,
#' or .zip will be automatically uncompressed.
#'
#' @return A data frame with the values of the csv file. If type of
#' \code{filename} is not correct or the file does not exist, an error message
#' is generated and nothing is returned.
#'
#' @examples
#' fars_read("accident_2015.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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

#' Make filenames for Fatality Analysis Reporting System
#'
#' The function \code{make_filename} generates filenames for the US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System
#' corresponding to a specific year.
#'
#' @param year A year as an integer.
#'
#' @return A filename for the Fatality Analysis Reporting System corresponding
#' to the specific year in the format "accident_<\code{year}>.csv.bz2".
#' If type of \code{year} is not correct, an error message is generated and
#' nothing is returned.
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Extract accidents per month and year
#'
#' The function \code{fars_read_years} reads data from the US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System
#' and extracts accidents per month and year.
#'
#' @param years A vector of years.
#'
#' @return A list of data frames. Each data frame corresponds to one year and
#' consits of columns for month and year as well as rows for accidents.
#' If type of \code{years} is not correct or if its elements are invalid,
#' an error message is generated and nothing is returned.
#'
#' @examples
#' fars_read_years(2013:2015)
#' fars_read_years(c(2013, 2015))
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df mutate select %>%
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

#' Count accidents per month and year
#'
#' The function \code{fars_summarize_years} reads data from the US National
#' Highway Traffic Safety Administration's Fatality Analysis Reporting System
#' and counts accidents per month and year.
#'
#' @param years A vector of years.
#'
#' @return A data frame with one column for all months and one column for each
#' year with the number of accidents per month and year. If type of \code{years}
#' is not correct or if its elements are invalid, an error message is generated
#' and nothing is returned.
#'
#' @examples
#' fars_summarize_years(2013:2015)
#' fars_summarize_years(c(2013, 2015))
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df mutate select group_by summarize %>%
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot accidents on US state map
#'
#' The function \code{fars_map_state} reads data from the US National Highway
#' Traffic Safety Administration's Fatality Analysis Reporting System and plots
#' all accidents of a specific state and year on a map.
#'
#' @param state.num A number of a state.
#' @param year A year.
#'
#' @return A plot is created where each accident of the specific state and the
#' specific year is shown on a map. In addition, \code{NULL} is returned. If the
#' type of \code{state.num} or \code{year} is not correct or their values
#' are invalid, an error message is generated and nothing is plotted
#' nor returned. If there are no accidents, an appropriate message is generated
#' and nothing is plotted again.
#'
#' @examples
#' fars_map_state(1, 2015)
#' fars_map_state(56, 2015)
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df filter %>%
#' @importFrom maps map
#' @importFrom graphics points
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
