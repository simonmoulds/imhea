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



compute_bias <- function(y, r1m) {
  abs(rev(y)[1] - sum(r1m[r1m > 0])) / rev(y)[1]
}

correct_bias <- function(x, y, x1m, r1m) {
  ## Use linear interpolation instead
  y2m = approx(x, y, x1m, rule=1)
  if (halves) {
    ## Zero rainfall rates at borders
    y2m[1] = 0
    y2m[length(y2m)] = rev(y2m)[2]
  }
  y2m
}

compute_rainfall_intensity <- function(y1m) {
  c(y1m[1], diff(y1m))
}

correct_negative_rate <- function(y, r1m, Lowint) {
  iter = 0
  while (iter <= 10 & (abs(rev(y)[1] - sum(r1m)) > Lowint | any(round(r1m[r1m != 0], digits = 8) < Lowint))) {
    iter = iter + 1
    r1m[r1m < 0] = 0 # Set negative rainfall rates to zero
    r1m[r1m > 0 & r1m < Lowint] = Lowint # Replace the lowest rates
    r1m[r1m >= Lowint] = r1m[r1m >= Lowint] * (rev(y)[1] - sum(r1m[r1m < Lowint])) / (sum(r1m) - sum(r1m[r1m < Lowint])) # Correction of biased rainfall
  }
  r1m
}

compute_cumulative_rainfall_curve <- function(y, r1m, halves) {
  ## Calculate the cumulative rainfall curve
  y2m = cumsum(r1m)
  ## Correct slight differences in totals
  y2m[length(y2m)] = rev(y)[1]
  if (halves) {
    ## Zero rainfall rates at borders
    y2m[length(y2m) - 1] = rev(y)[1]
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

fill_gaps <- function(Date1, P1, Date2, P2, cutend = FALSE) {
  ## %iMHEA Data gaps fill.
  ## % [NewDate1,NewP1,NewDate2,NewP2] = iMHEA_FillGaps(Date1,P1,Date1,P2,cutend,flag).
  ## %
  ## % Input:
  ## % Date1, Date2 = dd/mm/yyyy hh:mm:ss [date format].
  ## %                Both should be at the same temporal scale.
  ## % P1, P2       = Precipitation [mm].
  ## % cutend = Cut vectors not to fill gaps after the end [default: false].
  ## % flag   = leave empty NOT to run the data voids assessment and plots.
  ## %
  ## % Output:
  ## % NewDate1, 2   = dd/mm/yyyy hh:mm:ss [date format].
  ## % NewP1, NewP2  = Filled precipitation data [mm].

  ## ## FOR TESTING
  ## library(tidyverse)
  ## library(lubridate)
  ## file = "inst/extdata/iMHEA_raw/HUA/iMHEA_HUA_01_PD_01_raw.csv"
  ## x <- readr::read_csv(file)
  ## tz <- "Etc/GMT-5"
  ## try(tz <- lutz::tz_lookup_coords(lat, lon, method = "accurate"), silent = TRUE)
  ## options(digits.secs = 6)
  ## Date1 <- x[["Date"]] %>% as.POSIXct(tz = tz, format = "%d/%m/%Y %H:%M:%OS")
  ## P1 <- x[["Event mm"]]

  ## Check if data have the same temporal resolution
  scale1 = diff(Date1)
  scale2 = diff(Date2)
  if (median(scale1) > median(scale2)) {
    scale = round(median(scale1)) # Same temporal resolution
    aggregation(Date1, P1, scale) # TODO
    aggregation(Date2, P2, scale) # TODO
    ## [Date1,P1] = iMHEA_Aggregation(Date1,P1,scale);
    ## [Date2,P2] = iMHEA_Aggregation(Date2,P2,scale);
  } else if (median(scale2) > median(scale1)) {
    scale = round(median(scale2)) # Same temporal resolution
    aggregation(Date1, P1, scale) # TODO
    aggregation(Date2, P2, scale) # TODO
    ## [Date1,P1] = iMHEA_Aggregation(Date1,P1,scale);
    ## [Date2,P2] = iMHEA_Aggregation(Date2,P2,scale);
  } else {
    scale = round(median(scale1))
  } # FIXME

  ## TODO work out if this is necessary - I think it is only being used for the side-effect of plotting/logging
  ## %% VOID ASSESSMENT
  ## % Run data gap assessment and print inventory.
  ## fprintf('Data gap assessment of P1.\n')
  ## [~] = iMHEA_Voids(Date1,P1,1);
  ## fprintf('Data gap assessment of P2.\n')
  ## [~] = iMHEA_Voids(Date2,P2,1);

  ## CREATE UNIFIED DATE VECTOR AND ASSIGN CORRESPONDENT INPUT DATA
  nd = 1440 / scale # Number of intervals per day
  ## % Define initial and end dates and create single vector
  DI = min(Date1[1], Date2[1])
  DF = max(rev(Date1)[1], rev(Date2)[1])
  NewDate = seq(DI, DF, by = "1 min")
  ## Assign data when they correspond
  NewP1 = rep(NA, length(NewDate))
  NewP2 = rep(NA, length(NewDate))
  NewP1[match(Date1, NewDate)] = P1 # FIXME what if they don't match?
  NewP2[match(Date2, NewDate)] = P2 # FIXME as above
  ## Optionally, cut vectors not to fill gaps after then end
  if (cutend) {
    ## Identify the last non-NA data in both vectors
    indexnP1 = max(which(!is.na(P1)))
    indexnP2 = max(which(!is.na(P2))) # FIXME what if all NA values?
    indexndate = min(Date1[indexnP1], Date2[indexnP2])
    ## Cut vectors after the minimum of the identified dates
    NewP1 = NewP1[NewDate <= indexndate]
    NewP2 = NewP2[NewDate <= indexndate]
    NewDate = NewDate[NewDate <= indexndate]
  }

  ## TEST IF OVERLAPPING DATA EXIST
  ## Extract all the sections where NaN data exist in any of the vectors.
  auxP1 = NewP1
  auxP2 = NewP2
  auxP1 = auxP1[!(is.na(NewP1) | is.na(NewP2))]
  auxP2 = auxP2[!(is.na(NewP1) | is.na(NewP2))]
  ## Check if any of the vectors are empty
  if (length(auxP1) <= 1) {
    ## Restore data if cut before
    if (cutend) {
      NewDate = seq(DI, DF, by = "1 min")
      ## Assign data when they correspond
      NewP1 = rep(NA, length(NewDate))
      NewP2 = rep(NA, length(NewDate))
      NewP1[match(Date1, NewDate)] = P1
      NewP2[match(Date2, NewDate)] = P2
    }
    return(data.frame(NewDate, NewP1, NewP2))
  }
  ## Fill data gaps
  auxCumP1 = cumsum(auxP1)
  auxCumP2 = cumsum(auxP2)
  mod = lm(auxCumP1, aux)
  r2 = summary(mod)$r.squared
  ## Fill gaps only if the correlation is almost perfect
  if (R < 0.99) {
    if (cutend) {
      NewDate = seq(DI, DF, sep = "1 min")
      ## Assign data when they correspond
      NewP1 = rep(NA, length(NewDate))
      NewP2 = rep(NA, length(NewDate))
      NewP1[match(Date1, NewDate)] = P1
      NewP2[match(Date2, NewDate)] = P2
    }
    return(data.frame(NewDate, NewP1, NewP2))
  }
  ## FIXME - what is M?
  ## NewP1(isnan(NewP1)) = NewP2(isnan(NewP1))/M;
  ## NewP2(isnan(NewP2)) = NewP1(isnan(NewP2))*M;
  if (cutend) {
    ## % Assign data when they correspond
    NewP1[match(Date1, NewDate)] = P1
    NewP2[match(Date2, NewDate)] = P2
  }
  NewDate1 = data.frame(NewDate, NewP1, NewP2)
}

average <- function(Date, Q, scale) {
  ## %iMHEA Agregation of hydrological data (average within an interval).
  ## % [NewDate,NewQ,CumQ,VoidQ,MeanQ,MaxQ,MinQ] =
  ## % iMHEA_Average(Date,Q,scale,flag) averages discharge data.
  ## %
  ## % Input:
  ## % Date  = dd/mm/yyyy hh:mm:ss [date format].
  ## % Q     = Stage or Discharge [l/s, m3/s, mm].
  ## % scale = Agregation interval [min].
  ## % flag  = leave empty NOT to run the data voids assessment and plots.
  ## %
  ## % Output:
  ## % NewDate   = dd/mm/yyyy hh:mm:ss [date format] at specified interval.
  ## % NewQ      = Average stage or discharge [l/s, m3/s, mm].
  ## % CumQ      = Cumulative discharge [l/s, m3/s, mm].
  ## % VoidP     = Void intervals [l/s, m3/s, mm].
  ## % MeanQ     = Mean value for specified interval [l/s, m3/s, mm].
  ## % MaxQ      = Maximum value for specified interval [l/s, m3/s, mm].
  ## % MinQ      = Minimum value for specified interval [l/s, m3/s, mm].
  ## %
  ## % Boris Ochoa Tocachi
  ## % Imperial College London
  ## % Created in May, 2014
  ## % Last edited in November, 2017
  Date = Date - seconds(0.25)
  Voids = identify_voids(Date, Q)
  nd = 1440 / scale
  DI = ceiling_date(min(Date)) # Initial date
  DF = ceiling_date(max(Date)) # Final date
  NewDate = seq(DI, DF, by = "1 min")
  n = length(NewDate) # Number of intervals
  NewQ = rep(0, length(NewDate)) # Initialize aggregation
  Date = Date[!is.na(Q)]
  Q = Q[!is.na(Q)]
  k = length(Q) # Length of input data
  ## Set initial counter
  if (Date[1] == NewDate[1]) {
    j = 2
    NewQ[1] = Q[1]
  } else {
    j = 1
  }
  for (i in j:n) {
    l = 0 # Interval data counter
    ## Aggregate values
    ## while j<=k && nd*Date(j)<=NewDate(i) % && nd*Date(j)>NewDate(i-1)
    while (j <= k && Date[j] <= NewDate[i]) {
      NewQ[i] = NewQ[i] + Q[j]
      j = j+1
      l = l+1
    }
    NewQ[i] = NewQ[i] / l
  }
  ## Fill gaps between data when there is only one value missing
  for (i in 2:(n-1)) {
    if (is.na(NewQ[i])) {
      NewQ[i] = mean(c(NewQ[i-1], NewQ[i+1]))
    }
  }
  ## Fill remaining gaps with zeros to calculate cumQ
  NewQ[is.na(NewQ)] = 0

  ## PREPARE THE DATA VECTORS AT THE SPECIFIED SCALE
  CumQ = cumsum(NewQ) # Initialize accumulation
  VoidQ = NewQ
  for (i in 1:length(Voids)) {
    CumQ[NewDate > Voids[i,1] & NewDate < Voids[i,2]] = NA
    NewQ[NewDate > Voids[i,1] & NewDate < Voids[i,2]] = NA
  }
  VoidQ[!is.na(NewQ)] = NA
  ## Check initial and final values of Q for data existence
  if (NewQ[1] == 0 & NewQ[2] != 0) {
    VoidQ[1] = NewQ[1]
    NewQ[1] = NA
    CumQ[1] = NA
  }
  if (rev(NewQ)[1] == 0 && rev(NewQ)[2] != 0) {
    VoidQ[length(VoidQ)] = rev(NewQ)[1]
    NewQ[length(NewQ)] = NA
    CumQ[length(CumQ)] = NA
  }
  MeanQ = mean(NewQ, na.rm = TRUE)
  MaxQ = max(NewQ, na.rm = TRUE)
  MinQ = min(NewQ, na.rm = TRUE)
  return(data.frame(NewDate, NewQ, CumQ, VoidQ))
}

