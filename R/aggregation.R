#' Aggregate rainfall data
#'
#' Aggregate rainfall data with cubic spline interpolation.
#'
#' @param x rain_gauge.
#' @param timescale units. Aggregation interval in seconds [original - minutes]
#' @param bucket units. Rain gauge bucket volume in mm.
#' @param mintip logical. Whether to aggregate at 1-min intervals
#'   prior to interpolation.
#' @param halves logical. Whether to add zero rates at estimated
#'   event endpoints.
#' @param ... Additional arguments.
#'
#' @return tsibble
aggregation_cs <- function(x,
                           timescale = set_units(60, "s"),
                           bucket = set_units(0.2, "mm"),
                           mintip = TRUE,
                           halves = TRUE,
                           ...) {
  ## TODO can we handle the case where there is more than one key variable
  keys <- key_data(x)
  if (nrow(keys) > 1)
    stop("More than one key not currently supported")
  id <- keys[1, 1, drop=TRUE]

  Event_Date <- x[["Date"]]
  Event_mm <- x[["Event"]]
  event_units <- units(Event_mm)
  Event_mm <- Event_mm %>% as.numeric()

  ## User-defined variables
  ## TODO document these values or set as package options

  ## Minimum intensity to separate events: 0.2 mm h^-1 [Padron et al, 2015].
  Minint = set_units(0.2, "mm/h")

  ## Maximum intensity to merge events:
  ## 12.7 cm h^-1 [Onset, 2013], or
  ## 300 mm h^-1 [Manz, personal communication].
  Maxint = set_units(127, "mm/h")

  ## Intensity to distribute single tips: 3 mm h^{-1} [Wang et al, 2008].
  Meanint = set_units(3, "mm/h")

  ## Threshold intensity above which data is kept:
  ## 0.10 mm h^{-1} [Wang et al, 2008], or 1/2 Minint
  Lowint = min(set_units(0.1, "mm/h"), Minint / 2) # FIXME [mm min^{-1}]

  ## Move date by 0.25 seconds to avoid numerical or exportation errors.
  Event_Date = Event_Date - seconds(0.25)

  ## TODO move to helpers file
  is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
    abs(x - round(x)) < tol
  }
  if (!is_wholenumber(as.numeric(timescale))) {
    stop("Argument `timescale` should be an integer representing seconds.")
  }
  if (as.numeric(timescale) %% 60 != 0) {
    stop("Argument `timescale` should be a multiple of 60s (i.e. whole minutes)")
  }

  Voids = identify_voids(tibble(Date = Event_Date, Event = Event_mm))

  NewEvent_Date = Event_Date
  NewEvent_mm = Event_mm
  zero_ix <- (NewEvent_mm == 0) | is.na(NewEvent_mm)
  NewEvent_Date = NewEvent_Date[!zero_ix]
  NewEvent_mm = NewEvent_mm[!zero_ix]

  ## Maximum tip interval to separate events.
  ## N.B.
  ## * [bucket / Minint] gives the minimum time between tips in hours
  ## * multiplying by [60 * (1 / nd)] converts to days
  MaxT = set_units(bucket / Minint, seconds)
  ## Minimum tip interval to merge events.
  MinT = set_units(bucket / Maxint, seconds)
  ## Aggregate events to avoid large intensities
  if (mintip) {
    x = aggregate_events(NewEvent_Date, NewEvent_mm)
  } else {
    x = merge_events(NewEvent_Date, NewEvent_mm, MinT)
  }
  NewEvent_Date = x$Date
  NewEvent_mm = x$Prec

  ## Adding a supporting initial extreme to avoid crashing the code later.
  NewEvent_Date = c(Event_Date[1] - seconds(MaxT), NewEvent_Date)
  NewEvent_mm = c(0, NewEvent_mm)

  ## Redistribute rainfall tips occurring at relatively long periods.
  x = divide_events(NewEvent_Date, NewEvent_mm, MaxT)
  NewEvent_Date = x$Date
  NewEvent_mm = x$Prec
  ## Remove initial extreme to avoid crashing the code later
  ## FIXME - is this step necessary?
  NewEvent_Date = NewEvent_Date[2:length(NewEvent_Date)]
  NewEvent_mm = NewEvent_mm[2:length(NewEvent_mm)]

  NewEventDiff = set_units(int_length(int_diff(NewEvent_Date)), seconds) > MaxT
  ## Make first point the start of a new event
  NewEventDiff = c(TRUE, NewEventDiff)
  indx = which(NewEventDiff)

  ## Number of elements per event
  n = diff(indx) - 1
  n_last = length(NewEvent_Date) - rev(indx)[1]
  n = c(n, n_last)

  ## Duration of the events in seconds
  n_indx = length(indx)
  D = (NewEvent_Date[indx[2:n_indx] - 1] - NewEvent_Date[indx[1:(n_indx - 1)]])
  D_last = rev(NewEvent_Date)[1] - NewEvent_Date[indx[n_indx]]
  D = c(D, D_last)
  D = as.numeric(D, units = "mins")

  ## Index of events consisting of only 1 tip
  n1 = indx[n < 1]
  cat(sprintf("Number of rainfall events identified: %6i", length(indx)), "\n")
  cat(sprintf("Average duration of the events: %8.2f min", mean(D[D > 0])), "\n")
  cat(sprintf("Rainfall events consisting of 1 tip only: %6i", length(n1)), "\n")

  ## Build a 1 minute cumulative rainfall curve
  DI = floor_date(min(Event_Date), unit = "day")
  DF = ceiling_date(max(Event_Date), unit = "day")
  NewDate_1min = seq(DI, DF, by = "1 min")
  CumP_1min = set_units(rep(0, length(NewDate_1min)), mm)
  Single_1min = set_units(rep(0, length(NewDate_1min)), mm)
  biased = rep(0, length(n))
  bEvent = rep(0, length(n))

  ## CONSTANTS
  ## TODO make these accessible at the package level
  zero_mm <- set_units(0, mm)
  zero_secs <- set_units(0, secs)
  one_minute <- set_units(1, minute)

  cat(sprintf("Interpolating data..."), "\n")
  pb = txtProgressBar(min = 0, max = length(n), initial = 0)
  for (i in 1:length(n)) {
    ## Procedure:
    ## * Events with more than 2 points (fit a CS) [Wang et al, 2008].
    ## * Events with only 2 points (fit a line) [Ciach, 2003].
    ## * Events with only 1 points (distribute at a rate of 3 mm h^{-1}) [Wang et al, 2008].
    if (n[i] >= 1) {
      ## Relative time in seconds from the beginning of the event.
      x = (NewEvent_Date[indx[i] + (0:n[i])] - NewEvent_Date[indx[i]])
      x = set_units(as.numeric(x, units = "secs"), "s")
      ## Cumulative rainfall during the event
      y = cumsum(NewEvent_mm[indx[i]:(indx[i] + n[i])])
      y = set_units(y, "mm")
      if (halves) {
        ## Estimate initial point of the rainfall event
        ## Reduce half a second only to ensure correct initial data calculation
        x0 = bucket * (x[2] - x[1]) / (y[2] - y[1]) - set_units(0.5, "secs")
        xf = bucket * (rev(x)[1] - rev(x)[2]) / (rev(y)[1] - rev(y)[2])
        ## Allocate only 1-half tip at the start and end of event
        x = x + x0
        y = y - bucket / 2
        y = c(zero_mm, y, rev(y)[1] + bucket / 2)
        x = c(zero_secs, x, rev(x)[1] + xf)
        ## x = round(x) # TODO check - would floor/ceiling be better?
        x = ceiling(x)
        ## Aggregating data at 1-min interval starting at :00
        DI = max(
          DI,
          floor_date(NewEvent_Date[indx[i]] - seconds(x0), unit = "minute")
        )
        DF = ceiling_date(
          NewEvent_Date[indx[i] + n[i]] + seconds(xf),
          unit = "minute"
        )
        x1m <- seq(DI, DF, by = "1 min") - NewEvent_Date[indx[i]]
        ## Convert from difftime to seconds, and add offset
        x1m <- seconds(x1m) + seconds(x0)
        x1m = set_units(as.numeric(round(x1m)), "secs")
      } else {
        ## Aggregating data at 1-min interval starting at :00
        DI = max(
          DI,
          floor_date(NewEvent_Date[indx[i]] + seconds(0.5), unit = "minute")
        )
        DF = ceiling_date(NewEvent_Date[indx[i] + n[i]])
        x1m = seq(DI, DF, by = "1 min") - NewEvent_Date[indx[i]]
        x1m <- seconds(x1m)
        x1m <- set_units(as.numeric(round(x1m)), "secs")
      }
      ## TODO need to check these functions rigorously
      if (halves) {
        ## Set the estimated zero rate endpoints first derivatives to 0
        ## [Sadler and Busscher, 1989].
        y1m = pracma::cubicspline(
                        as.numeric(x),
                        as.numeric(y),
                        xi = as.numeric(x1m),
                        endp2nd = TRUE,
                        der = c(0, 0)
                      )
      } else {
        ## Set the endpoints second derivatives to 0 [Wang et al, 2008].
        ## N.B. a natural cubic spline is a cubic spline that sets second
        ## derivatives to zero at the end points.
        ## E.g. https://stats.stackexchange.com/q/322047
        ## pp = csape(x, y, 'second') # TODO
        y1m = spline(x, y, method = 'natural', xout = x1m)
        y1m = y1m$y
      }
      y1m <- set_units(y1m, "mm") # FIXME check this
      ## ## The above spline routines work as in the MATLAB original,
      ## ## but can result in negative rainfall intensities. Using a
      ## ## monotonic spline prevents this:
      ## y1m = spline(c(x[1] - 60, x, rev(x)[1] + 60), c(y[1], y, rev(y)[1]), method = 'hyman', xout = x1m)
      if (halves) {
        ## Zero rainfall rates at borders.
        y1m[1] <- 0
        y1m[length(y1m)] <- y1m[length(y1m) - 1]
      }
      r1m = compute_rainfall_intensity(y1m) # Rainfall rate at each x1m [mm min^{-1}]
      ## % Correction for negative intensities and biased volumes.
      r2m = r1m
      bEvent = 0
      bias = compute_bias(y, x1m, r1m)
      bEvent = bias > 0.25
      if (bEvent) {
        y2m = correct_bias(x, y, x1m, r1m, halves)
        r2m = compute_rainfall_intensity(y2m)
        bias = compute_bias(y, x1m, r2m)
      }
      r2m = correct_negative_rate(y, x1m, r2m, Lowint)
      y2m = compute_cumulative_rainfall_curve(y, r2m, halves)
      r2m = compute_rainfall_intensity(y2m)

      # Assemble cumulative rainfall curve
      ix = NewDate_1min >= DI & NewDate_1min <= DF
      CumP_1min[ix] = CumP_1min[ix] + y2m
      CumP_1min[NewDate_1min > DF] = CumP_1min[NewDate_1min == DF]
    } else {
      ## Aggregating data at 1-min interval starting at :00
      ## Meanint has units mm/h
      ## x0 is event duration in minutes
      ## xf is end of event in minutes
      x0 = set_units(NewEvent_mm[indx[i]], mm) / Meanint - one_minute
      xf = NewEvent_Date[indx[i]]
      ## Equally spaced divided tip [FIXME - equivalent to MATLAB?]
      x = seq(xf - minutes(round(set_units(x0, minute))), xf, by = "1 min")
      ## Initial date
      DI = floor_date(xf - minutes(round(set_units(x0, minute))), unit = "minute")
      ## Final date
      DF = ceiling_date(xf, unit = "minute")  # Final date in [min]
      ## Equally spaced time interval
      x1m = seq(DI, DF, by = "1 min")
      y <-
        (NewEvent_mm[indx[i]] * rep(1, length(x)) / (drop_units(set_units(x0, minute) + one_minute))) %>%
        set_units(mm) # FIXME make sure this is consistent

      ## Tip counting
      r1m = set_units(rep(0, length(x1m)), mm/minute)
      if (x[1] == x1m[1]) {
        j = 2 # Data counter
        r1m[1] = y[1] * set_units(1, 1/minute)
      } else {
        j = 1 # Data counter
      }
      for (itb in 2:length(x1m)) {
        ## Aggregate values
        while (j <= length(y) & x[j] > x1m[itb-1] & x[j] <= x1m[itb]) {
          r1m[itb] = r1m[itb] + y[j] * set_units(1, 1/minute)
          j = j + 1
        }
      }
      y1m = cumsum(r1m * set_units(1, minute))
      ## Assemble cumulative rainfall curve
      ix = NewDate_1min >= DI & NewDate_1min <= DF
      CumP_1min[ix] = CumP_1min[ix] + y1m
      CumP_1min[NewDate_1min > DF] = CumP_1min[NewDate_1min == DF]
      ## Assemble single tip rainfall vector
      Single_1min[ix] = Single_1min[ix] + y1m
      Single_1min[NewDate_1min > DF] = Single_1min[NewDate_1min == DF]
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  ## FIXME [logging]
  ## fprintf('Maximum bias corrected in event interpolation: %5.2f%%.\n',100*nanmax(biased))
  ## fprintf('%2i event(s) with bias >25%% interpolated linearly.\n',nansum(bEvent))
  ## fprintf('\n')
  ## % drawnow
  ## % delete(findall(0,'Type','figure'))

  ## Equally spaced time interval
  NewDate = seq(
    NewDate_1min[1],
    rev(NewDate_1min)[1],
    by = paste0(drop_units(set_units(timescale, "min")), " min")
  )
  ## nt <- length(NewDate)
  scale_int <- drop_units(set_units(timescale, minute))
  ix1 <- seq(scale_int + 1, length(CumP_1min), by = scale_int)
  ix0 <- seq(1, length(CumP_1min) - scale_int, by = scale_int)
  if (halves) {
    ## Rainfall rate at scale interval obtained from fitted cumulative rainfall
    NewP <- c(bucket / 2, (CumP_1min[ix1] - CumP_1min[ix0]))
    Single <- c(bucket / 2, (Single_1min[ix1] - Single_1min[ix0]))
  } else {
    ## Rainfall rate at scale interval obtained from fitted cumulative rainfall
    NewP <- c(zero_mm, CumP_1min[ix1] - CumP_1min[ix0])
    Single <- c(zero_mm, (Single_1min[ix1] - Single_1min[ix0]))
  }
  CumP = cumsum(NewP)
  ## Correct numerical errors in the calculations of zero rain rates.
  NewP[round(NewP, 8) == zero_mm] <- 0
  Single[round(Single,8) == zero_mm] <- 0
  ## Cut the vectors to the actual initial and final date.
  DI = ceiling_date(
    min(Event_Date),
    unit = paste0(drop_units(set_units(timescale, "min")), " min")
  )
  DF = ceiling_date(
    max(Event_Date),
    unit = paste0(drop_units(set_units(timescale, "min")), " min")
  )
  CumP[NewDate < DI | NewDate > DF] = NA
  NewP[NewDate < DI | NewDate > DF] = NA
  Single[NewDate < DI | NewDate > DF] = NA

  ## Incorporate data voids
  for (i in 1:nrow(Voids)) {
    idx = NewDate > Voids[i,1] & NewDate < Voids[i,2]
    CumP[idx] = NA
    NewP[idx] = NA
    Single[idx] = NA
  }
  cat(sprintf('Rainfall volume before aggregation: %8.2f mm.\n', sum(Event_mm, na.rm = TRUE)))
  cat(sprintf('Rainfall volume after aggregation: %8.2f mm.\n', sum(NewP, na.rm = TRUE)))
  ## out <- tibble(Date = NewDate, NewP=NewP, CumP=CumP, Single=Single)
  out <- tibble(Date = NewDate, Event = NewP, CumP = CumP, Single = Single)
  out <- out %>%
    filter(NewDate >= DI & NewDate <= DF)
  out <- tipping_bucket_rain_gauge(
    out,
    id,
    date_column = "Date",
    event_column = "Event",
    event_units = "mm"
  )
  ## out <- out %>% mutate(NewP = set_units(NewP, event_units, mm)
  out
}

## Helper for aggregation_cs(...)
aggregate_events <- function(Event_Date, Event_mm) {
  ## TODO convert this to Rcpp for speed
  ## Agregate rainfall at 1-min intervals.% Agregate rainfall at 1-min intervals.
  k <- length(Event_mm)
  DI <- floor_date(min(Event_Date), "day")   # Initial date [day]
  DF <- ceiling_date(max(Event_Date), "day") # Final date [day]
  NewDate_1min <- seq(DI, DF, by = "1 min")  # Equally spaced time interval
  n <- length(NewDate_1min)                  # Number of 1-min intervals
  NewP_1min <- rep(0, length(NewDate_1min))  # Initialise aggregation
  if (Event_Date[1] == NewDate_1min[1]) {
      j <- 2 # Data counter
      NewP_1min[1] <- Event_mm[1]
  } else {
      j <- 1 # Data counter
  }
  for (i in 2:n) {
    while ((j <= k) && (Event_Date[j] <= NewDate_1min[i])) {
      NewP_1min[i] <- NewP_1min[i] + Event_mm[j]
      j <- j+1
    }
  }
  zero_idx = NewP_1min == 0
  NewDate_1min = NewDate_1min[!zero_idx]
  NewP_1min = NewP_1min[!zero_idx]
  ## message(sprintf("Routine for aggregating tips at 1-min time interval"))
  ## message(sprintf("New number of data points: %4i", length(NewP_1min)))
  ## message(sprintf("Rainfall volume before aggregation: %8.2f mm", sum(Event_mm, na.rm = TRUE)))
  ## message(sprintf("Rainfall volume after aggregation: %8.2f mm", sum(NewP_1min, na.rm = TRUE)))
  d = tibble(Date = NewDate_1min, Prec = NewP_1min)
  return(d)
}

## Helper for aggregation_cs(...)
merge_events <- function(Event_Date, Event_mm, MinT) {
  ## Delete tips for small periods.
  initial_rainfall_volume <- sum(Event_mm, na.rm = TRUE)
  x =
    tibble(Date = Event_Date, Prec = Event_mm) %>%
    mutate(
      interval = set_units(c(int_length(int_diff(Date)), MinT * 100), "s"),
      event_count = seq(1, n())
    ) %>%
    mutate(event_count = ifelse(interval < MinT, lead(event_count), event_count))
  x_merged =
    x %>%
    group_by(event_count) %>%
    summarize(Prec = sum(Prec)) %>%
    left_join(
      x %>% dplyr::select(Date, event_count),
      by = "event_count"
    ) %>%
    dplyr::select(Date, Prec, -event_count)
  ## merged_rainfall_volume <- sum(x_merged$Prec, na.rm = TRUE)
  ## i = 1; j = 1 # FIXME
  ## message(sprintf('Routine for merging tips occurring faster than MinT = %6.2f seconds', MinT))
  ## message(sprintf('Number of tips removed: %4i.\n',i-j))
  ## message(sprintf('Rainfall volume before merging: %8.2f mm', initial_rainfall_volume))
  ## message(sprintf('Rainfall volume after merging: %8.2f mm', merged_rainfall_volume))
  x_merged
}

## Helper for aggregation_cs(...)
divide_events <- function(Event_Date, Event_mm, MaxT) {
  ## % Add additional tips for long periods [Wang et al, 2008].
  ## % Calculate the time between tips.
  ## Diff_Event_Date = diff(Event_Date);
  diff_event_date = set_units(int_length(int_diff(Event_Date)), "s")
  event_diff = diff_event_date > MaxT
  half_event_diff = diff_event_date > (MaxT / 2)
  ## Redistribute rainfall over relatively long periods
  ## i.e. lower than MaxT but greater than MaxT/2
  NewEvent_Date = Event_Date
  NewEvent_mm = Event_mm
  j = 1
  for (i in 2:length(event_diff)) {
    j = j + 1
    ## TODO check this index against MATLAB
    if (half_event_diff[i-1] & !event_diff[i-1] & (!event_diff[i] | !isTRUE(event_diff[i-2]))) {
      halftip_mm = Event_mm[i] / 2
      t0 = Event_Date[i] - seconds(diff_event_date[i-1] / 2)
      ## Include these data in the rainfall tip time series
      NewEvent_Date = c(
        NewEvent_Date[1:(j-1)],
        t0, NewEvent_Date[j:length(NewEvent_Date)]
      )
      NewEvent_mm = c(
        NewEvent_mm[1:(j-1)],
        halftip_mm, halftip_mm,
        NewEvent_mm[(j+1):length(NewEvent_mm)]
      )
      j = j + 1
    }
  }
  ## MaxT_minutes <- set_units(MaxT, "minute")
  ## message(sprintf("Routing for spreading tips occurring between %6.2f and %6.2f minutes", MaxT_minutes / 2, MaxT_minutes))
  ## message(sprintf("Number of tips added: %4i", j-i))
  ## message(sprintf("Rainfall volume before spreading: %8.2f mm", sum(Event_mm, na.rm = TRUE)))
  ## message(sprintf("Rainfall volume after spreading: %8.2f mm", sum(NewEvent_mm, na.rm = TRUE)))
  tibble(Date = NewEvent_Date, Prec = NewEvent_mm)
}

#' Aggregate data
#'
#' Temporal aggregation of hydrological time series data.
#'
#' @param x tsibble.
#' @param timescale units.
#' @param ... Additional arguments.
#'
#' @return tsibble
#'
aggregate <- function(x, timescale, ...) {
  UseMethod("aggregate")
}

#' @export
aggregate.stream_gauge <- function(x, timescale, ...) {
  x <-
    x %>%
    mutate(across(any_of(c("Q", "H")), as.numeric)) %>%
    group_by_key() %>%
    index_by(NewDate = ~ ceiling_date(., unit = paste0(as.numeric(set_units(timescale, "min")), " min"))) %>%
    summarize(across(any_of(c("Q", "H")), mean)) %>%
    rename(Date = NewDate)
  x <- x %>% tsibble::fill_gaps(.full = TRUE)
  x <- x %>% mutate(Q = zoo::na.approx(Q, maxgap = 1))
  ## TODO get discharge/stage units from options
  ## TODO pass x back to stream_gauge constructor
  x <- x %>% mutate(Q = set_units(Q, m3/s))
  if ("H" %in% names(x))
    x <- x %>% mutate(H = set_units(H, m))
  ## TODO pass back to the constructor
  ## TODO separate class for stage?
  class(x) <- c("stream_gauge", class(x))
  x
}

#' @export
aggregate.rain_gauge <- function(x, timescale, ...) {
  timescale_str <- paste0(as.numeric(set_units(timescale, "min")), " min")
  x <-
    x %>%
    mutate(across(any_of(c("Event")), as.numeric)) %>%
    group_by_key() %>%
    index_by(NewDate = ~ ceiling_date(., unit = timescale_str)) %>%
    summarize(across(any_of(c("Event")), sum)) %>%
    rename(Date = NewDate)
  x <- x %>% tsibble::fill_gaps(.full = TRUE)
  ## x <- x %>% mutate(Event = zoo::na.approx(Event, maxgap = 1))
  ## TODO fill with zeroes where NA, maxgap = 1
  ## x <- x %>% mutate(Event = set_units(Event, mm))
  ## x <- tipping_bucket_rain_gauge(
  ##   x, date_column = "Date", event_column = "Event", event_units = "mm"
  ## )
  ## class(x) <- c("rain_gauge", class(x))
  class(x) <- c("tipping_bucket_rain_gauge", class(x))
  x
}

#' Daily aggregation
#'
#' Convenience function to aggregate catchment data to daily temporal
#' resolution.
#'
#' @param x tsibble.
#' @param ... Additional arguments.
#'
#' @return tsibble
#'
aggregate_daily <- function(x, ...) {
  UseMethod("aggregate_daily")
}

## #' @export
## aggregate_daily.catchment <- function(x, ...) {
##   x_daily <-
##     x %>%
##     mutate(across(any_of(c("Q", "H", "Event")), as.numeric)) %>%
##     group_by_key() %>%
##     index_by(NewDate = ~ as_date(.)) %>%
##     summarize(
##       ## n = sum(is.na(Q)),
##       across(any_of(c("Event")), sum, na.rm = TRUE),
##       across(any_of(c("Q", "H")), mean, na.rm = TRUE)
##       ## Q = mean(Q),
##       ## H = mean(H),
##       ## Event = sum(Event, na.rm = TRUE)
##     ) %>%
##     rename(Date = NewDate)
##   ## class(x_daily) <- c("catchment", class(x))
##   x_daily
## }

#' @export
aggregate_daily.tbl_ts <- function(x, ...) {
  x_daily <-
    x %>%
    mutate(across(any_of(c("Q", "H", "Event")), as.numeric)) %>%
    group_by_key() %>%
    index_by(NewDate = ~ as_date(.)) %>%
    summarize(
      across(any_of(c("Event")), sum, na.rm = TRUE),
      across(any_of(c("Q", "H")), mean, na.rm = TRUE)
    ) %>%
    rename(Date = NewDate)
  x_daily
}

#' Hourly aggregation
#'
#' Convenience function to aggregate catchment data to hourly temporal
#' resolution.
#'
#' @param x tsibble.
#' @param ... Additional arguments.
#'
#' @return tsibble
#'
aggregate_hourly <- function(x, ...) {
  UseMethod("aggregate_hourly")
}

## #' @export
## aggregate_hourly.catchment <- function(x, ...) {
##   x_hourly <-
##     x %>%
##     mutate(across(any_of(c("Q", "H", "Event")), as.numeric)) %>%
##     group_by_key() %>%
##     index_by(NewDate = ~ ceiling_date(., unit = "1 hour")) %>%
##     summarize(
##       ## n = sum(is.na(Q)),
##       across(any_of(c("Event")), sum, na.rm = TRUE),
##       across(any_of(c("Q", "H")), mean, na.rm = TRUE)
##     ) %>%
##     rename(Date = NewDate)
##   x_hourly
## }

#' @export
aggregate_hourly.tbl_ts <- function(x, ...) {
  x_hourly <-
    xx %>%
    mutate(across(any_of(c("Q", "H", "Event")), as.numeric)) %>%
    group_by_key() %>%
    index_by(NewDate = ~ ceiling_date(., unit = "1 hour")) %>%
    summarize(
      across(any_of(c("Event")), sum, na.rm = TRUE),
      across(any_of(c("Q", "H")), mean, na.rm = TRUE)
    ) %>%
    rename(Date = NewDate)
  x_hourly
}

#' Monthly aggregation
#'
#' Convenience functions
#'
#' @param x catchment
#' @param ... Additional arguments.
#'
#' @return tsibble
#'
aggregate_monthly <- function(x, ...) {
  UseMethod("aggregate_monthly")
}

## #' @export
## aggregate_monthly.catchment <- function(x, ...) {
##   x_monthly <-
##     x %>%
##     mutate(across(any_of(c("Q", "H", "Event")), as.numeric)) %>%
##     group_by_key() %>%
##     index_by(NewDate = ~ yearmonth(.)) %>%
##     summarize(
##       ## n = sum(is.na(Q)),
##       across(any_of(c("Event")), sum, na.rm = TRUE),
##       across(any_of(c("Q", "H")), mean, na.rm = TRUE)
##       ## Q = mean(Q),
##       ## H = mean(H),
##       ## Event = sum(Event, na.rm = TRUE)
##     ) %>%
##     rename(Date = NewDate)
##   ## class(x_monthly) <- c("catchment", class(x))
##   x_monthly
## }

#' @export
aggregate_monthly.tbl_ts <- function(x, ...) {
  x_monthly <-
    x %>%
    mutate(across(any_of(c("Q", "H", "Event")), as.numeric)) %>%
    group_by_key() %>%
    index_by(NewDate = ~ yearmonth(.)) %>%
    summarize(
      across(any_of(c("Event")), sum, na.rm = TRUE),
      across(any_of(c("Q", "H")), mean, na.rm = TRUE)
    ) %>%
    rename(Date = NewDate)
  x_monthly
}

#' Annual aggregation
#'
#' Convenience functions
#'
#' @param x catchment
#' @param ... Additional arguments.
#'
#' @return tsibble
#'
aggregate_annual <- function(x, ...) {
  UseMethod("aggregate_annual")
}

## #' @export
## aggregate_annual.catchment <- function(x, ...) {
##   x_annual <-
##     x %>%
##     mutate(across(any_of(c("Q", "H", "Event")), as.numeric)) %>%
##     group_by_key() %>%
##     index_by(NewDate = ~ year(.)) %>%
##     summarize(
##       ## n = sum(is.na(Q)),
##       across(any_of(c("Event")), sum, na.rm = TRUE),
##       across(any_of(c("Q", "H")), mean, na.rm = TRUE)
##       ## Q = mean(Q, na.rm = TRUE),
##       ## H = mean(H, na.rm = TRUE),
##       ## Event = sum(Event, na.rm = TRUE)
##     ) %>%
##     rename(Date = NewDate)
##   ## class(x_annual) <- c("catchment", class(x))
##   x_annual
## }

#' @export
aggregate_annual.tbl_ts <- function(x, ...) {
  x_annual <-
    x %>%
    mutate(across(any_of(c("Q", "H", "Event")), as.numeric)) %>%
    group_by_key() %>%
    index_by(NewDate = ~ year(.)) %>%
    summarize(
      across(any_of(c("Event")), sum, na.rm = TRUE),
      across(any_of(c("Q", "H")), mean, na.rm = TRUE)
    ) %>%
    rename(Date = NewDate)
  x_annual
}

## ## #' export
## ## aggregate.rain_gauge <- function(x, timescale, ...)
## aggregation <- function(Date, P, timescale, ...) {
##   ## Agregation of rainfall within an interval
##   ##
##   ## Input:
##   ## Date  = dd/mm/yyyy hh:mm:ss [date format].
##   ## P     = Precipitation [mm].
##   ## scale = Agregation interval [min].
##   ## flag  = leave empty NOT to run the data voids assessment and plots.
##   ##
##   ## Output:
##   ## NewDate   = dd/mm/yyyy hh:mm:ss [date format] at specified interval.
##   ## NewP      = Agregated Precipitation [mm].
##   ## CumP      = Cumulative rainfall [mm].
##   ## VoidP     = Void intervals [mm].
##   ## MaxP      = Maximum intensity for specified interval [mm].
##   Date = Date - seconds(0.25)
##   Voids = identify_voids(tibble(Date = Date, Event = P))
##   DI = ceiling_date(min(Date)) # Initial date
##   DF = ceiling_date(max(Date)) # Final date
##   timescale_str <- paste0(as.numeric(set_units(timescale, "min")), " min")
##   NewDate = seq(DI, DF, by = timescale_str)
##   n = length(NewDate) # Number of intervals
##   NewP = rep(0, length(NewDate)) # Initialize aggregation
##   zero_ix <- (P == 0) | is.na(P)
##   Date = Date[!zero_ix]
##   P = P[!zero_ix]
##   k = length(P) # Length of input data
##   ## Set initial counter
##   if (Date[1] == NewDate[1]) {
##     j = 2
##     NewP[1] = P[1]
##   } else {
##     j = 1
##   }
##   for (i in j:n) {
##     ## Aggregate values
##     ## while j<=k && nd*Date(j)<=NewDate(i) % && nd*Date(j)>NewDate(i-1)
##     while ((j <= k) && (Date[j] <= NewDate[i])) {
##       NewP[i] = NewP[i] + P[j]
##       j = j + 1
##     }
##   }
##   ## Fill gaps between data when there is only one value missing
##   for (i in 2:(n-1)) {
##     if (is.na(NewP[i])) {
##       NewP[i] = 0
##     }
##   }
##   CumP <- cumsum(NewP)
##   ## Placing data gaps again in the aggregated vectors
##   VoidP <- NewP
##   ## Incorporate data voids
##   if (nrow(Voids) > 0) {
##     for (i in 1:nrow(Voids)) {
##       idx = NewDate > Voids[i,1] & NewDate < Voids[i,2]
##       CumP[idx] = NA
##       NewP[idx] = NA
##     }
##   }
##   ## VoidP[!is.na(NewP)] <- NA
##   ## ## Correct the last row
##   ## if (rev(NewP)[1] == 0 && (is.na(rev(NewP)[2]))) {
##   ##   VoidP[length(VoidP)] <- NewP[length(NewP)]
##   ##   NewP[length(NewP)] <- NA
##   ##   CumP[length(NewP)] <- NA
##   ## }
##   ## MaxP <- max(NewP, na.rm = TRUE) # Maximum intensity
##   tibble(Date = NewDate, Prec = NewP, CumP = CumP)
## }
