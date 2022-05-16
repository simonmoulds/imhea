#' Load rain_gauge object
#'
#' Load iMHEA precipitation object from file.
#'
#' @param file Character. Path to input precipitation data.
#' @param tz Character. Timezone of recorded times. Defaults to
#'   "Etc/GMT-5" (i.e. timezone of Peru and Ecuador)
#' @param lat Numeric.
#' @param lon Numeric.
#' @param gauge_type Character.
#' @param bucket_volume Numeric vector.
#' @param date_column Character. The name of the date column. Default
#'   is "Date"
#' @param event_column Character. The name of the event column.
#'   Default is "Event mm"
#' @param ... Additional arguments.
#'
#' @return A precipitation object.
#'
#' @examples
#' \dontrun{
#' sum(1:10)
#' }
rain_gauge <- function(file,
                       tz = "Etc/GMT-5",
                       lat = NA,
                       lon = NA,
                       gauge_type,
                       bucket_volume = 0.2,
                       date_column = "Date",
                       event_column = "Event mm") {
  ## ## FOR TESTING:
  ## file = "inst/extdata/iMHEA_raw/HUA/iMHEA_HUA_01_PD_01_raw.csv"
  x <- readr::read_csv(file)
  tz <- "Etc/GMT-5"
  try(tz <- lutz::tz_lookup_coords(lat, lon, method = "accurate"), silent = TRUE)
  times <- x[[date_column]] %>% as.POSIXct(tz = tz, format = "%d/%m/%Y %H:%M:%S")
  events <- x[[event_column]]
  events_cleaned <- remove_consecutive_tips(events, times)
}

new_rain_gauge <- function(...) {
  ## Constructor function
  rain_gauge(...)
}

validate_rain_gauge <- function(x) {
  ## Validate rain gauge object
  x
}

remove_consecutive_tips <- function(events, times) {
  ## Remove consecutive tips above maximum intensity
  ## Clone of iMHEA_Depure.m
  ##
  nd = 86400
  min_tip_interval = 1.1 / nd # TODO make this a package option
  ## Time between tips
  intervals = int_length(int_diff(times))
  ## Add arbitrary long interval at the beginning of the time series
  intervals = c(min_tip_interval * 100, intervals)
  ## Identify tips separated by less than min_tip_interval
  idx = which(intervals <= min_tip_interval) + 1
  events[idx] = 0.
  ## %% PRINT RESULTS
  ## fprintf('Removing tips occurring faster than MinT = %6.2f seconds.\n',MinT*86400)
  ## fprintf('Number of tips identified: %4i.\n',length(find(EventDiff)))
  ## fprintf('Rainfall volume before depuration: %8.2f mm.\n',nansum(Event_mm))
  ## fprintf('Rainfall volume after depuration: %8.2f mm.\n',nansum(NewEvent_mm))
  ## fprintf('\n')
  events
}

rain_gauge_network <- function(...,
                               target_resolution = 1) {
  ## TODO logging
  dots = list(...)
  n_gauges = length(dots)
}



compute_bias <- function(y, x1m, r1m) {
  ## y is cumulative rainfall, so rev(y)[1] is the total rainfall during the event
  ## r1m <- set_units(r1m, mm/minute)
  tot0 <- max(y)
  zero_ix <- r1m > set_units(0, mm/h)
  tot1 <- sum(r1m[zero_ix] * set_units(1, minute))
  bias <- (abs(tot0 - tot1) / tot0) %>% drop_units()
  bias
}

correct_bias <- function(x, y, x1m, r1m) {
  ## Use linear interpolation instead
  y2m <- approx(x, y, x1m, rule=1)$y
  y2m <- y2m %>% set_units(mm)
  if (halves) {
    ## Zero rainfall rates at borders
    y2m[1] = set_units(0, mm)
    y2m[length(y2m)] = rev(y2m)[2]
  }
  y2m
}

## compute_rainfall_intensity <- function(y1m) {
##   int <- set_units(c(y1m[1], diff(y1m)), "mm/h") # FIXME check this is correct
## }
compute_rainfall_intensity <- function(y1m) {
  ## y1m is cumulative rainfall at minute intervals
  r1m <- c(y1m[1], diff(y1m)) / set_units(1, "minute")
  r1m <- set_units(r1m, "mm/h")
  r1m
}

correct_negative_rate <- function(y, x1m, r1m, Lowint) {
  iter = 0
  zero_mmh <- set_units(0, mm/h)
  one_minute <- set_units(1, minute)
  mycond <- function(y, r1m, iter, ...) {
    tot0 <- max(y)
    tot1 <- sum(r1m * one_minute)
    cond1 <- iter <= 10
    cond2 <- abs(tot0 - tot1) > (Lowint * one_minute)
    ## FIXME slightly confusing with the units here - r1m/r2m always in mm/minute?
    cond3 <- any(round(r1m[r1m != zero_mmh], digits = 8) < Lowint)
    cond1 & (cond2 | cond3)
  }
  myfun <- function(r1m) {
    sum(r1m * set_units(1, minute))
  }
  while (mycond(y, r1m, iter)) {
    iter = iter + 1
    r1m[r1m < zero_mmh] = zero_mmh # Set negative rainfall rates to zero
    r1m[r1m > zero_mmh & r1m < Lowint] = Lowint # Replace the lowest rates
    correction_factor <- (
      (max(y) - sum(r1m[r1m < Lowint] * one_minute))
      / (sum(r1m * one_minute) - sum(r1m[r1m < Lowint] * one_minute))
    )
    ## Correction of biased rainfall
    r1m[r1m >= Lowint] = r1m[r1m >= Lowint] * correction_factor
  }
  r1m
}

compute_cumulative_rainfall_curve <- function(y, r1m, halves) {
  ## Calculate the cumulative rainfall curve
  y2m = cumsum(r1m * set_units(1, minute))
  ## Correct slight differences in totals
  y2m[length(y2m)] = max(y)
  if (halves) {
    ## Zero rainfall rates at borders
    y2m[length(y2m) - 1] = max(y)
  }
  y2m
  ## ## Recalculate the intensities
  ## r1m = c(y2m[1], diff(y2m))
  ## r1m
}

intCorrection <- function(r1m, y, Lowint, halves, x, x1m) {
  ## function [r2m,y2m,biased,bEvent] = intCorrection(r1m,y,Lowint,halves,x,x1m)
  ## % Only keep rain rates above the threshold.
  r2m = r1m
  bEvent = 0
  bias = compute_bias(y, r1m)
  bEvent = bias > 0.25
  if (bEvent) {
    y2m = correct_bias(x, y, x1m, r1m)
    r2m = compute_rainfall_intensity(y2m)
    bias = compute_bias(y, r2m)
  }
  r2m = correct_negative_rate(y, r2m, Lowint)
  y2m = compute_cumulative_rainfall_curve(y, r2m, halves)
  r2m = compute_rainfall_intensity(y2m)
  ## if (biased > 0.25) {
  ##   bEvent = 1
  ##   ## Use linear interpolation instead
  ##   y2m = approx(x, y, x1m, rule=1)
  ##   if (halves) {
  ##     ## Zero rainfall rates at borders
  ##     y2m[1] = 0
  ##     y2m[length(y2m)] = rev(y2m)[2]
  ##   }
  ##   ## Rainfall rate at each x1m
  ##   r2m = c(y2m[1], diff(y2m))
  ##   ## Recalculate bias
  ##   biased = compute_bias(y, r2m)
  ## }
  ## iter = 0
  ## while (iter <= 10 & (abs(rev(y)[1] - sum(r2m)) > Lowint | any(round(r2m[r2m != 0], digits = 8) < Lowint))) {
  ##   iter = iter + 1
  ##   r2m[r2m < 0] = 0 # Set negative rainfall rates to zero
  ##   r2m[r2m > 0 & r2m < Lowint] = Lowint # Replace the lowest rates
  ##   r2m[r2m >= Lowint] = r2m[r2m >= Lowint] * (rev(y)[1] - sum(r2m[r2m < Lowint])) / (sum(r2m) - sum(r2m[r2m < Lowint])) # Correction of biased rainfall
  ## }
  ## ## Calculate the cumulative rainfall curve
  ## y2m = cumsum(r2m)
  ## ## Correct slight differences in totals
  ## y2m[length(y2m)] = rev(y)[1]
  ## if (halves) {
  ##   ## Zero rainfall rates at borders
  ##   y2m[length(y2m) - 1] = rev(y)[1]
  ## }
  ## ## Recalculate the intensities
  ## r2m = c(y2m[1], diff(y2m))
  ## list(r2m = r2m, y2m = y2m$y, biased = biased, bEvent = bEvent)
}

aggregation <- function(Date, P, Scale, flag) {
  ## %iMHEA Agregation of rainfall (agregation within an interval).
  ## % [NewDate,NewP,CumP,VoidP,MaxP] = iMHEA_Aggregation(Date,P,scale,flag)
  ## % aggregates precipitation data using tipping bucket counting.
  ## %
  ## % Input:
  ## % Date  = dd/mm/yyyy hh:mm:ss [date format].
  ## % P     = Precipitation [mm].
  ## % scale = Agregation interval [min].
  ## % flag  = leave empty NOT to run the data voids assessment and plots.
  ## %
  ## % Output:
  ## % NewDate   = dd/mm/yyyy hh:mm:ss [date format] at specified interval.
  ## % NewP      = Agregated Precipitation [mm].
  ## % CumP      = Cumulative rainfall [mm].
  ## % VoidP     = Void intervals [mm].
  ## % MaxP      = Maximum intensity for specified interval [mm].

  ## % Move date by 0.25 seconds to avoid numerical errors.
  Date = Event - seconds(0.25)
  Voids = identify_voids(Date, P)
  P[is.na(P)] = 0
  ## % Transform date variables for easier processing.
  ## Date = datenum(Date);

  ## %% AGGREGATION
  nd = 1440 / scale
  DI = ceiling_date(min(Date)) # Initial date
  DF = ceiling_date(max(Date)) # Final date
  NewDate = seq(DI, DF, by = "1 min")
  n = length(NewDate) # Number of intervals
  NewP = rep(0, length(NewDate)) # Initialize aggregation
  ## Delete zero events
  indx = (P == 0)
  Date = Date[P > 0]
  P = P[P > 0]
  k = length(P)

  ## if nd*(Date(1)) == NewDate(1)
  if (Date[1] == NewDate[1]) {
    j = 2 # Data counter
    NewP[1] = P[1]
  } else {
    j = 1 # Data counter
  }

  for (i in j:n) {
    ## Aggregate values
    while (j <= k & Date[j] <= NewDate[i]) {
      NewP[i] = NewP[i] + P[j]
      j = j + 1
    }
  }

  ## Fill gaps between data when there is only one value missing
  for (i in 2:(n-1)) {
    if (is.na(NewP[i])) {
      NewP[i] = 0 # FIXME
    }
  }

  ## %% PREPARE THE DATA VECTORS AT THE SPECIFIED SCALE
  CumP = cumsum(NewP) # Initialize accumulation
  ## NewDate = datetime(NewDate/nd,'ConvertFrom','datenum'); % Rescale the date
  ## Date = datetime(Date,'ConvertFrom','datenum'); % Restore the original date
  ## Return data gaps to aggregated vectors
  VoidP = NewP
  for (i in 1:length(Voids)) {
    CumP[NewDate > Voids[i,1] & NewDate < Voids[i,2]] = NA
    NewP[NewDate > Voids[i,1] & NewDate < Voids[i,2]] = NA
  }
  VoidP[!is.na(NewP)] = NA
  ## Correct the last row
  if (rev(NewP)[1] == 0 & is.na(rev(NewP)[2])) {
    VoidP[length(VoidP)] = rev(NewP)[1]
    NewP[length(NewP)] = NA
    CumP[length(CumP)] = NA
  }
  MaxP = max(NewP, na.rm = TRUE) # Maximum intensity
  return(data.frame(NewDate, NewP, CumP, VoidP))
}
