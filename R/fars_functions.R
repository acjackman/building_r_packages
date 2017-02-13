t# Call to globalVariables to appease R CHECK
if(getRversion() >= "2.15.1") utils::globalVariables(c(
    "MONTH", "LATITUDE", "LONGITUD", "STATE", "year", "n"))

#' Read raw Fatality Analysis Reporting System File
#'
#' Read a single year of accident data into an R \code{data_frame}. Format a
#' single years raw data downloaded from US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System into a R object.
#' The file will be automatically decompressed if it uses the bz2 compression
#' algorithm.
#'
#' If the file doesn't exist, an error will be raised.
#'
#' @param filename A character string with the full path to the CSV file.
#'
#' @return This function returns a \code{data_frame} object with the same
#'     column names used in the input data file.
#'
#' @examples
#' \dontrun{
#' # Read uncompressed file
#' fars_read('data/accident_2015.csv')
#' # Read compressed file
#' fars_read('data/accident_2015.csv.bz2')
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generate the default filename for a given year of data
#'
#' Generate the filename used by the Fatality Analysis Reporting System for
#' the given years worth of data and return as a string.
#'
#' @param year The four digit year of data to retrieve.
#'
#' @return The default filename as a string.
#'
#' @export
#' @examples
#' make_filename(2013)
#' make_filename(2014L)
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read Multiple Years of Accident Data
#'
#' Read multiple years of Fatality Analysis Reporting System accident data to
#' produce a list of \code{data_frames} with one accident per record with the
#' month and year of the accident.
#'
#' @param years A vector of years.
#'
#' @return A list of \code{data_frames} containing the month and year of all
#'    accidents. If a year without data is provided, a warning will show and
#'    that place in the list will be \code{NULL}.
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
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

#' Summarize the number of accidents across years.
#'
#' Summarize the number of accidents per month across multiple years.
#'
#' @param years A vector of years.
#'
#' @return A \code{data_frame} with one record per month, and columns for each
#'         year selected.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013))
#' fars_summarize_years(c(2013, 2014, 2015))
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom magrittr %>%
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


#' Show map of accidents in a state.
#'
#' Plot all accidents for a single year on a map.
#'
#' @param state.num state number in the
#' @param year data year to create a map for
#'
#' @examples
#' \dontrun{
#' fars_map_state(35, 2014)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @importFrom maps map
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
