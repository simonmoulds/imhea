#' Remove consecutive tips
#'
#' Remove consecutive tips above maximum intensity
#'
#' @param x tipping_bucket_rain_gauge
#' @param ... Additional arguments.
#'
#' @return tibble
depure <- function(x, ...) {
  UseMethod("depure")
}

#' @export
depure.tipping_bucket_rain_gauge <- function(x, ...) {
  ## Clone of iMHEA_Depure.m
  ## https://github.com/tidyverse/design/issues/42 - discussion on verbosity
  min_tip_interval = 1.1 # TODO make this a package option
  initial_rainfall_volume = x$Event %>% sum(na.rm = TRUE)
  ## We add arbitrary long interval at the beginning of the time series
  start_interval = min_tip_interval * 100
  ## int_length(...) ALWAYS returns the length of the interval returned by int_diff(...) in seconds
  x = x %>% mutate(Interval = c(start_interval, int_length(int_diff(Date))))
  ## Identify tips separated by less than min_tip_interval
  x = x %>% mutate(Event = ifelse(Interval <= min_tip_interval, 0, Event))
  ## %% PRINT RESULTS
  depured_rainfall_volume = x$Event %>% sum(na.rm = TRUE)
  message(sprintf("Removing tips occurring faster than `min_tip_interval` = %6.2f seconds", min_tip_interval))
  message(sprintf("Number of tips identified: %4i.", length(which(x$Interval <= min_tip_interval))))
  message(sprintf("Rainfall volume before depuration: %8.2f mm", initial_rainfall_volume))
  message(sprintf("Rainfall volume after depuration: %8.2f mm", depured_rainfall_volume))
  x
}
