#' Load rain_gauge object
#'
#' @param x tibble. Raw data.
#' @param id Character. Gauge ID.
#' @param date_column Character. Name of column holding time information.
#' @param date_format Character. Date format.
#' @param tz Character. Timezone of recorded times. Defaults to
#'   "Etc/GMT-5" (i.e. timezone of Peru and Ecuador)
#' @param event_column Character. The name of the event column.
#'   Default is "Event mm"
#' @param event_units Character. Precipitation units (e.g. mm).
#' @param flag_column Character. The name of the flag column. Default
#'   is "Flag"
#' @param ... Additional arguments.
#'
#' @return A rain_gauge object.
#'
#' @examples
#'
#' p_raw = read_csv(
#'   system.file("extdata", "LLO/iMHEA_LLO_01_PO_01_raw.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' p <- p_raw %>% rain_gauge(id = "LLO_01_P0_01", event_units = "mm")
#'
#' is_rain_gauge(p)
#'
#' @export
rain_gauge <- function(x,
                       id,
                       date_column = "Date",
                       date_format = "%d/%m/%Y %H:%M:%S",
                       tz = "Etc/GMT-5",
                       event_column = "Event mm",
                       event_units,
                       flag_column = "Flag",
                       ## raw = TRUE,
                       ...) {

  ## TODO assume that flags will be handled appropriately as a pre-processing step
  stopifnot(date_column %in% names(x))
  stopifnot(event_column %in% names(x))
  stopifnot(!missing(event_units))
  x <- x %>%
    dplyr::rename(Date = date_column, P = event_column) %>%
    dplyr::select(Date, P) %>%
    dplyr::mutate(ID = id, .before = "Date")
  x <- x %>%
    imhea_to_tsibble(date_column, date_format, tz, regular = FALSE)
  x <- x %>% depure() %>% dplyr::select(-Interval)
  x <- x %>%
    dplyr::mutate(P = units::set_units(P, event_units, mode = "standard")) %>%
    dplyr::mutate(P = units::set_units(P, mm))
  tsibble::new_tsibble(x, class = "rain_gauge")
}

#' @rdname rain_gauge
#' @export
is_rain_gauge <- function(x) {
  isTRUE(inherits(x, "rain_gauge"))
}
