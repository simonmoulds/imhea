#' Load stream_gauge object
#'
#' @inheritParams rain_gauge
#' @param discharge_column. Character. Name of column holding discharge data.
#'   Default is "Flow l/s"
#' @param discharge_units. Character. Discharge units (e.g. l/s, m3/s).
#' @param level_column. Character. Name of column holding level data.
#' @param level_units. Character. Level units (e.g. m, mm).
#'
#' @return A stream_gauge object.
#'
#' @examples
#'
#' q_raw <- read_csv(
#'   system.file("extdata", "LLO/iMHEA_LLO_01_HI_01_raw.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' q <-
#'   q_raw %>%
#'   stream_gauge(
#'     id = "LLO_01_HI_01",
#'     discharge_units = "l/s",
#'     level_column = "Level cm",
#'     level_units = "cm"
#'   )
#'
#' is_stream_gauge(q)
#'
#' @export
stream_gauge <- function(x,
                         id,
                         date_column = "Date",
                         date_format = "%d/%m/%Y %H:%M:%S",
                         tz = "Etc/GMT-5",
                         discharge_column = "Flow l/s",
                         discharge_units,
                         level_column = NA,
                         level_units,
                         flag_column = "Flag",
                         raw = TRUE,
                         ...) {

  ## TODO allow users to set standard measurement units in options
  stopifnot(date_column %in% names(x))
  stopifnot(discharge_column %in% names(x))
  stopifnot(is.na(level_column) | isTRUE(level_column %in% names(x)))
  x <- x %>% dplyr::rename(Date = date_column, Q = discharge_column)
  if (is.na(level_column)) {
    x <- x %>% dplyr::select(Date, Q) #, Flag)
  } else {
    x <- x %>% dplyr::rename(H = level_column) %>% dplyr::select(Date, Q, H) #, Flag)
  }
  x <- x %>% dplyr::mutate(ID = id, .before = "Date")
  x <- x %>%
    imhea_to_tsibble(date_column, date_format, tz, regular = FALSE)
  x <- x %>%
    dplyr::mutate(Q = units::set_units(Q, discharge_units, mode = "standard")) %>%
    dplyr::mutate(Q = units::set_units(Q, m3/s))
  if ("H" %in% names(x))
    x <- x %>%
      dplyr::mutate(H = units::set_units(H, level_units, mode = "standard")) %>%
      dplyr::mutate(H = units::set_units(H, m))
  tsibble::new_tsibble(x, class = "stream_gauge")
}

#' @rdname stream_gauge
#' @export
is_stream_gauge <- function(x) {
  isTRUE(inherits(x, "stream_gauge"))
}

valid_timezones <- function() return(OlsonNames())

imhea_to_tsibble <- function(x,
                             date_column = "Date",
                             date_format = "%d/%m/%Y %H:%M:%S",
                             tz = "Etc/GMT-5",
                             regular,
                             ...) {
  stopifnot(date_column %in% names(x))
  stopifnot(tz %in% valid_timezones())
  x <- x %>% dplyr::rename(Date = date_column)
  if (is.character(x$Date))
    x <-
      x %>%
      dplyr::mutate(Date = as.POSIXct(Date, tz = tz, format = date_format))
  x <-
    x %>%
    dplyr::arrange(Date) %>%
    dplyr::filter(!tsibble::are_duplicated(., key = ID, index = Date)) %>%
    tsibble::as_tsibble(key = ID, index = Date, regular = regular)
  x
}

depure <- function(x, ...) {
  ## Clone of iMHEA_Depure.m
  ## https://github.com/tidyverse/design/issues/42 - discussion on verbosity
  min_tip_interval = 1.1 # TODO make this a package option
  initial_rainfall_volume = x$P %>% sum(na.rm = TRUE)
  ## We add arbitrary long interval at the beginning of the time series
  start_interval = min_tip_interval * 100
  ## int_length(...) ALWAYS returns the length of the interval returned by int_diff(...) in seconds
  x = x %>% dplyr::mutate(Interval = c(start_interval, lubridate::int_length(lubridate::int_diff(Date))))
  ## Identify tips separated by less than min_tip_interval
  x = x %>% dplyr::mutate(P = ifelse(Interval <= min_tip_interval, 0, P))
  ## %% PRINT RESULTS
  depured_rainfall_volume = x$P %>% sum(na.rm = TRUE)
  ## message(sprintf("Removing tips occurring faster than `min_tip_interval` = %6.2f seconds", min_tip_interval))
  ## message(sprintf("Number of tips identified: %4i.", length(which(x$Interval <= min_tip_interval))))
  ## message(sprintf("Rainfall volume before depuration: %8.2f mm", initial_rainfall_volume))
  ## message(sprintf("Rainfall volume after depuration: %8.2f mm", depured_rainfall_volume))
  x
}
