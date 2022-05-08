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
  ## ## FOR TESTING:
  ## library(tidyverse)
  ## library(lubridate)
  ## file = "inst/extdata/iMHEA_raw/HUA/iMHEA_HUA_01_PD_01_raw.csv"
  ## x <- readr::read_csv(file)
  ## tz <- "Etc/GMT-5"
  ## try(tz <- lutz::tz_lookup_coords(lat, lon, method = "accurate"), silent = TRUE)
  ## options(digits.secs = 6)
  ## Event_Date <- x[["Date"]] %>% as.POSIXct(tz = tz, format = "%d/%m/%Y %H:%M:%OS")
  ## Event_mm <- x[["Event mm"]]
  ## Event_mm <- remove_consecutive_tips(Event_mm, Event_Date)
  Event_Date <- x[["Date"]]
  Event_mm <- x[["Event"]]

  ## User-defined variables - TODO document these values or set as package options
  ## Minimum intensity to separate events: 0.2 mm h^-1 [Padron et al, 2015].
  Minint = set_units(0.2, "mm/h")
  ## Maximum intensity to merge events:
  ## 12.7 cm h^-1 [Onset, 2013], or
  ## 300 mm h^-1 [Manz, personal communication].
  Maxint = set_units(127, "mm/h")

  ## Intensity to distribute single tips: 3 mm h^{-1} [Wang et al, 2008].
  Meanint = set_units(3, "mm/h")

  ## Threshold intensity above which data is kept:
  ## 0.10 mm h^{-1} [Wang et al, 2008], or
  ## 1/2 Minint
  Lowint = min(set_units(0.1, "mm/h"), Minint / 2) # FIXME [mm min^{-1}]

  ## Move date by 0.25 seconds to avoid numerical or exportation errors.
  Event_Date = Event_Date + seconds(0.25)
  ## % Add zero rates at estimated initial and final event times, using (Vb/2):
  ## % 1 half rate and 1 half partial tip [Sadler and Busscher, 1989].
  ## % halves = true; % [default: true]

  ## Initialize variables
  ## TODO logging -
  ## fprintf('\n')
  ## fprintf('RAINFALL AGGREGATION USING CUBIC SPLINE INTERPOLATION.\n')

  ## TODO move to helpers file
  is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
    abs(x - round(x)) < tol
  }
  if (!is_wholenumber(as.numeric(timescale))) {
    stop("Argument `timescale` should be an integer representing seconds.")
  }

  ## %% IDENTIFY DATA GAPS
  ## if nargin > 6
  ##     % Run data gap assessment and plot inventory.
  ##     [Voids] = iMHEA_Voids(Event_Date,Event_mm,1,1);
  ##     % Plot tips
  ##     plot(Event_Date(Event_mm~=0),Event_mm(Event_mm~=0),'o','DisplayName','Events')
  ##     set(gca,'YTickLabel',[],'Ylim',[-2 max(Event_mm)+1])
  ##     legend('off'); legend('show');
  ##     legend('location','SouthWest'); legend('boxoff')
  ## else
  ##     % Run data gap assessment.
  ##     [Voids] = iMHEA_Voids(Event_Date,Event_mm,1);
  ## end
  Voids = identify_voids(tibble(Date = Event_Date, Event = Event_mm))

  ## %% TRANFORM DATES TO NUMBERS FOR EASIER PROCESSING - TODO check if necessary
  ## % Modified variables to process.
  ## Event_Date = Event_Date # datenum(Event_Date)
  NewEvent_Date = Event_Date
  NewEvent_mm = Event_mm
  NewEvent_Date = NewEvent_Date[!is.na(NewEvent_mm)]
  NewEvent_mm = NewEvent_mm[!is.na(NewEvent_mm)]

  ## %% PREPROCESS RAINFALL EVENTS
  ## nd = 1440; # % Number of minutes per day or numeric value of 1 minute: 1/1440
  ## Maximum tip interval to separate events.
  ## N.B.
  ## * [bucket / Minint] gives the minimum time between tips in hours
  ## * multiplying by [60 * (1 / nd)] converts to days
  ## * we actually want the units in seconds, so instead of multiplying by [60 * (1 / nd)], we multiply by 3600
  ## MaxT = 60 * (1 / nd) * bucket / Minint;
  ## ## % Minimum tip interval to merge events.
  ## MinT = 60 * (1 / nd) * bucket / Maxint;
  MaxT = set_units(bucket / Minint, "s")
  ## % Minimum tip interval to merge events.
  MinT = set_units(bucket / Maxint, "s")
  ## % Aggregate events to avoid large intensities
  if (mintip) {
    ## Aggregate data at 1-min scale.
    x = aggregate_events(NewEvent_Date, NewEvent_mm)
  } else {
    ## % Merge rainfall tips occurring at extremely short periods.
    x = merge_events(NewEvent_Date, NewEvent_mm, MinT)
  }
  ## TODO work with dataframes throughout
  NewEvent_Date = x$Date
  NewEvent_mm = x$Prec
  ## if mintip == true
  ##     % Aggregate data at 1-min scale.
  ##     [NewEvent_Date,NewEvent_mm] = AggregateEvents(NewEvent_Date,NewEvent_mm);
  ## else
  ##     % Merge rainfall tips occurring at extremely short periods.
  ##     [NewEvent_Date,NewEvent_mm] = MergeEvents(NewEvent_Date,NewEvent_mm,MinT);
  ## end
  ## % Adding a supporting initial extreme to avoid crashing the code later.
  NewEvent_Date = c(Event_Date[1] - seconds(MaxT), NewEvent_Date)
  NewEvent_mm = c(0, NewEvent_mm)
  ## NewEvent_Date = cat(1,Event_Date(1)-MaxT,NewEvent_Date);
  ## NewEvent_mm = cat(1,0,NewEvent_mm);
  ## % Redistribute rainfall tips occurring at relatively long periods.
  x = divide_events(NewEvent_Date, NewEvent_mm, MaxT)
  ## FOR TESTING:
  x <-
    read_csv("~/dev/imhea/matlab_divide_events_output.csv", col_names = FALSE, show_col_types = FALSE) %>%
    setNames(c("Date", "Prec")) %>%
    mutate(Date = (Date - 719529) * 86400) %>%
    mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
    mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
    mutate(Date = force_tz(Date, "Etc/GMT-5"))
  NewEvent_Date = x$Date
  NewEvent_mm = x$Prec
  ## Remove initial extreme to avoid crashing the code later - FIXME
  NewEvent_Date = NewEvent_Date[2:length(NewEvent_Date)]
  NewEvent_mm = NewEvent_mm[2:length(NewEvent_mm)]

  NewEventDiff = set_units(int_length(int_diff(NewEvent_Date)), "s") > MaxT
  NewEventDiff = c(TRUE, NewEventDiff) # Make first point the start of a new event
  indx = which(NewEventDiff)
  ## y = read_csv("matlab_indx_output.csv", col_names = FALSE)[,1,drop = T]
  ## Again, slight differences because of precision errors
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
  n1 = indx[n < 1] # Index of events with only 1 point
  cat(sprintf("Number of rainfall events identified: %6i", length(indx)), "\n")
  cat(sprintf("Average duration of the events: %8.2f min", mean(D[D > 0])), "\n")
  cat(sprintf("Rainfall events consisting of 1 tip only: %6i", length(n1)), "\n")

  ## FIT EVENTS AND AGGREGATING AT 1-min INTERVAL
  ## Build a 1 minute cumulative rainfall curve
  DI = floor_date(min(Event_Date), unit = "minute")
  DF = ceiling_date(max(Event_Date), unit = "minute")
  NewDate_1min = seq(DI, DF, by = "1 min")
  CumP_1min = set_units(rep(0, length(NewDate_1min)), mm)   # Initialise accumulation
  Single_1min = set_units(rep(0, length(NewDate_1min)), mm) # Initialise single tip counting
  biased = rep(0, length(n))                              # Initialise bias vector
  bEvent = rep(0, length(n))                              # Initialise biased events counter

  ## CONSTANTS
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
        x = round(x) # TODO check - would floor/ceiling be better?
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
      ## % CS fitted to the current event and interpolated at 1-sec.
      ## % pp = spline(x,y); % yy = spline(x,y,xx);
      ## ## Experimenting with pracma::cubicspline
      ## ## Examples from https://uk.mathworks.com/help/matlab/ref/spline.html
      ## x = c(0, 1, 2.5, 3.6, 5, 7, 8.1, 10)
      ## y = sin(x)
      ## xx = seq(0, 10, by=.25)
      ## yy = cubicspline(x, y, xi=xx)

      ## with endpoints:
      ## x = seq(-4, 4, by = 1)
      ## y = c(0, .15, 1.12, 2.36, 2.36, 1.46, .49, .06, 0)
      ## xx = seq(-4, 4, length.out = 101)
      ## yy = cubicspline(x, y, xi=xx, endp2nd = TRUE, der = c(0, 0)) # OK!
      ## yy = spline(x, y, method='natural', xout=xx)

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
      ## Check
      ## plot(x, y)
      ## lines(x1m, y1m)
      ## r1m = [y1m(1);diff(y1m)]; % Rainfall rate at each x1m [mm min^{-1}]
      r1m = compute_rainfall_intensity(y1m) # Rainfall rate at each x1m [mm min^{-1}]
      ## % Correction for negative intensities and biased volumes.
      r2m = r1m
      bEvent = 0
      bias = compute_bias(y, x1m, r1m)
      bEvent = bias > 0.25
      if (bEvent) {
        y2m = correct_bias(x, y, x1m, r1m)
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
      ## TODO [seems this section is only needed for plotting, which I think is probably unnecessary...]
      ## if (any(is.nan(r2m))) {
      ##   ## Calculations for event plots
      ##   ## TODO
      ##   xx = 0:x[length(x)]
      ##   yy = fnval(pp, xx)
      ##   ## Aggregating data at 1-min interval
      ##   ## Alternatively starting at x[1]
      ##   x1m = 60 * (1:floor(length(xx) / 60)) - 1 # Equally spaced time interval
      ##   y1m = yy[60 * (1:floor(length(xx) / 60))]
      ##   r1m = 60 * (yy[60 * (1:floor(length(xx) / 60))] - yy[60 * (1:floor(length(xx) / 60)) - 59])
      ##   ## Linear sections of the current event and interpolated at 1-sec
      ##   y3m = interp1(x, y, x1m, 'linear', 0) # Cumulative rainfall at each x1m
      ##   if (halves) {
      ##     ## Zero rainfall rates at borders
      ##     y3m[1] = 0
      ##     y3m[length(y3m)] = y3m[length(y3m) - 1]
      ##   }
      ##   r3m = [y3m[1]:diff(y3m)] # Rainfall rate at each x1m [mm min^{-1}]
      ##   ## Correction for biased volumes TODO
      ##   ## [r4m,y4m] = intCorrection(r3m,y,Lowint,halves,x,x1m);

      ##   ## Tip counting
      ##   r5m = rep(0, length(x1m)) # Initialize aggregation
      ##   p = c(y[1], diff(y))
      ##   if (x[1] == x1m[1]) {
      ##     j = 2 # Data counter
      ##     r5m[1] = p[1]
      ##   } else {
      ##     j = 1 # Data counter
      ##   }
      ##   for (itb in 2:length(x1m)) {
      ##     ## Aggregate values
      ##     while (j <= length(p) & x[j] > x1m[itb-1] & x[j] <= x1m[itb]) {
      ##       r5m[itb] = r5m[itb] + p[j]
      ##       j = j + 1
      ##     }
      ##   }
      ##   y5m = cumsum(r5m) # Accumulation
      ##   ## % Plot events.
      ##   ## figure(199)
      ##   ## subplot(2,1,1)
      ##   ## plot(x,y,'o',x1m,y5m,':',x1m,y4m,'-.',x1m,y1m,'-.',x1m,y2m,'-.')
      ##   ## set(gca,'Xlim',[-60 x(end)+60])
      ##   ## title(['Rainfall event number ',num2str(i)])
      ##   ## xlabel('Time in seconds from the beggining of the event [s]')
      ##   ## ylabel('Cumulative rainfall [mm]')
      ##   ## legend('Rain gauge tip','1-min tip counting','1-min linear corrected',...
      ##   ##     '1-min CS interpolated','1-min CS corrected',...
      ##   ##     'location','NorthWest')
      ##   ## legend('boxoff')
      ##   ## subplot(2,1,2)
      ##   ## plot(x,60*[y(1);diff(y)],'o',x1m,60*r5m,':',x1m,60*r4m,'-.',x1m,60*r1m,'-.',x1m,60*r2m,'-.')
      ##   ## set(gca,'Xlim',[-60 x(end)+60])
      ##   ## xlabel('Time in seconds from the beggining of the event [s]')
      ##   ## ylabel('Rainfall rate [mm h^{-1}]')
      ##   ## legend('Rain gauge tip','1-min tip counting','1-min linear corrected',...
      ##   ##     '1-min CS interpolated','1-min CS corrected',...
      ##   ##     'location','NorthWest')
      ##   ## legend('boxoff')
      ##   ## % Add breakpoint to check plots.
      ## }
    } else {
      ## x0 = bucket * (x[2] - x[1]) / (y[2] - y[1]) - 0.5
      ## xf = bucket * (rev(x)[1] - rev(x)[2]) / (rev(y)[1] - rev(y)[2])
      ## ## Allocate only 1-half tip at the start and end of event
      ## x = x + x0
      ## y = y - bucket / 2
      ## y = c(0, y, rev(y)[1] + bucket / 2)
      ## x = c(0, x, rev(x)[1] + xf)
      ## x = round(x) # TODO check - would floor/ceiling be better?

      ## ## Aggregating data at 1-min interval starting at :00
      ## DI = max(DI, floor_date(NewEvent_Date[indx[i]] - x0, unit = "minute"))
      ## DF = ceiling_date(NewEvent_Date[indx[i] + n[i]] + xf)
      ## x1m = seq(DI, DF, by = "1 min") - NewEvent_Date[indx[i]] + x0 # Equally spaced time interval

      ## Aggregating data at 1-min interval starting at :00
      ## Meanint has units mm/h
      ## x0 is event duration in minutes
      ## xf is end of event in minutes
      x0 = set_units(NewEvent_mm[indx[i]], mm) / Meanint
      xf = NewEvent_Date[indx[i]]
      ## Equally spaced divided tip [FIXME - equivalent to MATLAB?]
      ## x = (xf - x0 * nd / 1440:xf)
      x = seq(xf - minutes(set_units(x0, minute)), xf, by = "1 min")
      ## Initial date
      DI = floor_date(xf - minutes(set_units(x0, minute)), unit = "minute")
      ## Final date
      DF = ceiling_date(xf, unit = "minute")  # Final date in [min]
      ## Equally spaced time interval
      x1m = seq(DI, DF, by = "1 min")
      y <- (
        NewEvent_mm[indx[i]]
        / rep(1 / (drop_units(set_units(x0, minute)) + 1), length(x))
      ) %>% set_units(mm) # FIXME make sure this is consistent
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
  nt <- length(NewDate)
  if (halves) {
    ## Rainfall rate at scale interval obtained from fitted cumulative rainfall
    NewP <- c(bucket / 2, (CumP_1min[2:nt] - CumP_1min[1:(nt - 1)]))
    Single <- c(bucket / 2, (Single_1min[2:nt] - Single_1min[1:(nt - 1)]))
  } else {
    ## Rainfall rate at scale interval obtained from fitted cumulative rainfall
    NewP <- c(zero_mm, CumP_1min[2:nt] - CumP_1min[1:(nt - 1)])
    Single <- c(zero_mm, (Single_1min[2:nt] - Single_1min[1:(nt - 1)]))
  }
  print(sum(NewP, na.rm=T))
  ## Cumulative rainfall at scale interval.
  CumP = cumsum(NewP)
  ## % Correct numerical errors in the calculations of zero rain rates.
  NewP[round(NewP, 8) == zero_mm] <- 0
  Single[round(Single,8) == zero_mm] <- 0
  ## Cut the vectors to the actual initial and final date.
  DI = ceiling_date(
    min(Event_Date),
    unit = paste0(drop_units(set_units(timescale, "min")), " min")
  )
  ## DI = ceiling_date(min(Event_Date) * nd) # % Initial date in [min]
  DF = ceiling_date(
    max(Event_Date),
    unit = paste0(drop_units(set_units(timescale, "min")), " min")
  ) # % Final date in [min]
  CumP[NewDate < DI | NewDate > DF] = NA
  NewP[NewDate < DI | NewDate > DF] = NA
  Single[NewDate < DI | NewDate > DF] = NA
  NewDate[NewDate < DI | NewDate > DF] = NA
  ## NewDate = NewDate / 1440 # % Rescale the date
  ## % Example (or validation) for 1 minute aggregation.
  ## % NewDate = NewDate_1min/1440;
  ## % CumP = CumP_1min;
  ## % NewP = [CumP(1);diff(CumP)];
  for (i in 1:length(Voids)) {
    idx = NewDate > Voids[i,1] & NewDate < Voids[i,2]
    CumP[idx] = NA
    NewP[idx] = NA
    Single[idx] = NA
  }
  print(sprintf('Rainfall volume before aggregation: %8.2f mm.\n', sum(Event_mm, na.rm = TRUE)))
  print(sprintf('Rainfall volume after aggregation: %8.2f mm.\n', sum(NewP, na.rm = TRUE)))
  sprintf('\n')
  ## TODO return something - data.frame? See what iMHEA_Workflow is expecting
  tibble(Date = NewDate, NewP=NewP, CumP=CumP, Single=Single)
}

## x = c(1, 2, 9, 9, 2, 1, 1, 5, 5, 1)
## x = c(1, 2, 9, NA, 2, 1, NA, 5, 5, 1)
## localMin2 <- function(x) {
##   finite_ix = which(!is.na(x))
##   x = x[finite_ix]
##   arms = diff(c(-.Machine$integer.max, -x)) > 0
##   index.arms = cumsum(rle2(arms)$lengths)
##   end.arms.true = index.arms[seq.int(from = 1, to = length(index.arms), by = 2)]
##   if (x[1] == x[2]) {
##       end.arms.true = end.arms.true[-1]
##   }
##   end.arms.true = finite_ix[end.arms.true]
##   return(end.arms.true)
## }
## localMin2(x)

aggregate_events <- function(Event_Date, Event_mm) {
  ## Agregate rainfall at 1-min intervals.% Agregate rainfall at 1-min intervals.
  k = length(Event_mm)
  DI = floor_date(min(Event_Date), "day") #* nd #% Initial date [day]
  DF = ceiling_date(max(Event_Date), "day") # * nd #% Final date [day]
  NewDate_1min = seq(DI, DF, by = "1 min") #' # % Equally spaced time interval
  print(length(NewDate_1min))
  n = length(NewDate_1min) #; % Number of 1-min intervals
  NewP_1min = rep(0, length(NewDate_1min)) # % Initialise aggregation
  if (Event_Date[1] == NewDate_1min[1]) {
      j = 2 # Data counter
      NewP_1min[1] = Event_mm[1]
  } else {
      j = 1 # Data counter
  }
  for (i in 2:n) {
    ## Aggregate values
    while (j <= k & Event_Date[j] <= NewDate_1min[i]) {
      NewP_1min[i] = NewP_1min[i] + Event_mm[j]
      j = j+1
    }
  }
  zero_idx = NewP_1min == 0
  NewDate_1min = NewDate_1min[!zero_idx]
  NewP_1min = NewP_1min[!zero_idx]
  ## initial_rainfall_volume = sum(Event_mm, na.rm = TRUE)
  ## x =
  ##   tibble(Date = Event_Date, Prec = Event_mm) %>%
  ##   mutate(Date = floor_date(Event_Date, unit = "minute")) %>%
  ##   group_by(Date) %>%
  ##   summarise(Prec = sum(Prec))
  ## aggregated_rainfall_volume <- sum(x$Prec, na.rm = TRUE)
  message(sprintf("Routine for aggregating tips at 1-min time interval"))
  message(sprintf("New number of data points: %4i", length(NewP_1min)))
  message(sprintf("Rainfall volume before aggregation: %8.2f mm", sum(Event_mm, na.rm = TRUE)))
  message(sprintf("Rainfall volume after aggregation: %8.2f mm", sum(NewP_1min, na.rm = TRUE)))
  tibble(Date = NewDate_1min, Prec = NewP_1min)
}



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
  merged_rainfall_volume <- sum(x_merged$Prec, na.rm = TRUE)
  i = 1; j = 1 # FIXME
  message(sprintf('Routine for merging tips occurring faster than MinT = %6.2f seconds', MinT))
  message(sprintf('Number of tips removed: %4i.\n',i-j))
  message(sprintf('Rainfall volume before merging: %8.2f mm', initial_rainfall_volume))
  message(sprintf('Rainfall volume after merging: %8.2f mm', merged_rainfall_volume))
  x_merged
  ## ## Length of time between tips in seconds
  ## Diff_Event_Date = int_length(int_diff(Event_Date))
  ## EventDiff = which(Diff_Event_Date <= MinT)
  ## NewEvent_Date = Event_Date
  ## NewEvent_mm = Event_mm
  ## ## Create a grouping variable
  ## merged_events = seq(1, length(Event_Date))
  ## merged_events[EventDiff] = NA

  ## ## % Calculate the time between tips.
  ## ## Diff_Event_Date = diff(Event_Date);
  ## ## % Identify tips separated by less than the minimum time MinT.
  ## ## EventDiff = Diff_Event_Date <= MinT;
  ## ## % Modified variables to process.
  ## ## NewEvent_Date = Event_Date;
  ## ## NewEvent_mm = Event_mm;
  ## ## i = 0; j = 0; % Initialise counters
  ## ## while any(EventDiff)
  ## ##     % Reassign rainfall to the following interval.
  ## ##     j = 1; n =length(NewEvent_Date);
  ## ##     for i = 2:n
  ## ##         j = j+1;  % Index for NewEvent_mm.
  ## ##         % When the time between tips is less than MaxT.
  ## ##         if EventDiff(i-1)
  ## ##             % Aggregate the tip volume to the following time stamp.
  ## ##             NewEvent_mm(j) = NewEvent_mm(j)+NewEvent_mm(j-1);
  ## ##             % Include these data in the rainfall tip time series.
  ## ##             NewEvent_mm(j-1) = [];
  ## ##             NewEvent_Date(j-1) = [];
  ## ##             j = j-1;
  ## ##         end
  ## ##     end
  ## ##     % Calculate the time between tips.
  ## ##     Diff_Event_Date = diff(NewEvent_Date);
  ## ##     % Identify tips separated by less than the minimum time MinT.
  ## ##     EventDiff = Diff_Event_Date <= MinT;
  ## ## end
  ## ## fprintf('Routine for merging tips occurring faster than MinT = %6.2f seconds.\n',MinT*86400)
  ## ## fprintf('Number of tips removed: %4i.\n',i-j)
  ## ## fprintf('Rainfall volume before merging: %8.2f mm.\n',nansum(Event_mm))
  ## ## fprintf('Rainfall volume after merging: %8.2f mm.\n',nansum(NewEvent_mm))
  ## ## fprintf('\n')
  ## NULL
}

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
  MaxT_minutes <- set_units(MaxT, "minute")
  ## % Identify tips separated by more than the maximum time MaxT.
  ## EventDiff = Diff_Event_Date > MaxT;
  ## % Redistribute rainfall over relatively long periods but lower than MaxT.
  ## % but greater than half MaxT.
  ## HalfEventDiff = Diff_Event_Date > MaxT/2;
  ## % Modified variables to process.
  ## NewEvent_Date = Event_Date;
  ## NewEvent_mm = Event_mm;
  ## j = 1;
  ## for i = 2:length(EventDiff)
  ##     j = j+1;  % Index for NewEvent_mm.
  ##     % When the time between tips is between half and one complete MaxT.
  ##     % Be aware that ~EventDiff(i-2) may conflict with i = 2.
  ##     if HalfEventDiff(i-1) && ~EventDiff(i-1) && (~EventDiff(i) || ~EventDiff(i-2))
  ##         % Divide the following tip volume in two and assign a time stamp.
  ##         Halftip_mm = Event_mm(i)/2;
  ##         t0 = Event_Date(i) - Diff_Event_Date(i-1)/2;
  ##         % Include these data in the rainfall tip time series.
  ##         NewEvent_Date = cat(1,NewEvent_Date(1:j-1),t0,NewEvent_Date(j:end));
  ##         NewEvent_mm = cat(1,NewEvent_mm(1:j-1),Halftip_mm,Halftip_mm,NewEvent_mm(j+1:end));
  ##         j = j+1;
  ##     end
  ## end
  ## fprintf('Routine for spreading tips occurring between %6.2f and %6.2f minutes.\n',MaxT*1440/2,MaxT*1440)
  ## fprintf('Number of tips added: %4i.\n',j-i)
  ## fprintf('Rainfall volume before spreading: %8.2f mm.\n',nansum(Event_mm))
  ## fprintf('Rainfall volume after spreading: %8.2f mm.\n',nansum(NewEvent_mm))
  ## fprintf('\n')
  message(sprintf("Routing for spreading tips occurring between %6.2f and %6.2f minutes", MaxT_minutes / 2, MaxT_minutes))
  message(sprintf("Number of tips added: %4i", j-i))
  message(sprintf("Rainfall volume before spreading: %8.2f mm", sum(Event_mm, na.rm = TRUE)))
  message(sprintf("Rainfall volume after spreading: %8.2f mm", sum(NewEvent_mm, na.rm = TRUE)))
  tibble(Date = NewEvent_Date, Prec = NewEvent_mm)
}
