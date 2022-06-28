## TODO add this to package
valid_timezones <- function() return(OlsonNames())

imhea_to_tsibble <- function(x,
                             date_column = "Date",
                             date_format = "%d/%m/%Y %H:%M:%S",
                             tz = "Etc/GMT-5",
                             regular,
                             ...) {
  stopifnot(date_column %in% names(x))
  stopifnot(tz %in% valid_timezones())
  x <- x %>% rename(Date = date_column)
  if (is.character(x$Date))
    x <-
      x %>%
      mutate(Date = as.POSIXct(Date, tz = tz, format = date_format))

  x <-
    x %>%
    arrange(Date) %>%
    filter(!are_duplicated(., key = ID, index = Date)) %>%
    as_tsibble(key = ID, index = Date, regular = regular)
  x
}

#' @export
tipping_bucket_rain_gauge <- function(x,
                                      id,
                                      date_column = "Date",
                                      date_format = "%d/%m/%Y %H:%M:%S",
                                      tz = "Etc/GMT-5",
                                      event_column = "Event mm",
                                      event_units,
                                      flag_column = "Flag",
                                      raw = TRUE,
                                      ...) {

  ## TODO assume that flags will be handled appropriately as a pre-processing step
  stopifnot(date_column %in% names(x))
  stopifnot(event_column %in% names(x))
  stopifnot(!missing(event_units))
  x <- x %>%
    rename(Date = date_column, Event = event_column) %>%
    dplyr::select(Date, Event) %>%
    mutate(ID = id, .before = "Date")
  x <- x %>%
    imhea_to_tsibble(date_column, date_format, tz, regular = FALSE)
  x <- x %>% depure() %>% dplyr::select(-Interval)
  x <- x %>%
    mutate(Event = set_units(Event, event_units, mode = "standard")) %>%
    mutate(Event = set_units(Event, mm))
  if (!raw) {
    class(x) <- c("tipping_bucket_rain_gauge", class(x))
    return(x)
  }
  class(x) <- c("tipping_bucket_rain_gauge", class(x))
  return(x)
}

#' @export
stream_gauge <- function(x,
                         id,
                         date_column = "Date",
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

  x <- x %>% rename(Date = date_column, Q = discharge_column) #, Flag = flag_column)
  if (is.na(level_column)) {
    x <- x %>% dplyr::select(Date, Q) #, Flag)
  } else {
    x <- x %>% rename(H = level_column) %>% dplyr::select(Date, Q, H) #, Flag)
  }
  x <- x %>% mutate(ID = id, .before = "Date")
  x <- x %>%
    imhea_to_tsibble(date_column, ..., regular = FALSE)
  x <- x %>%
    mutate(Q = set_units(Q, discharge_units, mode = "standard")) %>%
    mutate(Q = set_units(Q, m3/s))
  if ("H" %in% names(x))
    x <- x %>%
      mutate(H = set_units(H, level_units, mode = "standard")) %>%
      mutate(H = set_units(H, m))
  class(x) <- c("stream_gauge", class(x))
  return(x)
}
