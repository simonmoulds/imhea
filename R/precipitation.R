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
                       lat,
                       lon,
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

## function [NewDate,NewP,CumP,Single] = iMHEA_AggregationCS(Event_Date,Event_mm,scale,bucket,mintip,halves,varargin)
## %iMHEA Cubic spline interpolation rainfall aggregation.
## % [NewDate,NewP,CumP,Single] =
## % iMHEA_AggregationCS(Event_Date,Event_mm,scale,Vb,mintip,halves,flag)
## % aggregates precipitation data using cublic spline interpolation.
## %
## % Input:
## % Event_Date= dd/mm/yyyy hh:mm:ss [date format].
## % Event_mm  = Precipitation [mm].
## % scale     = Agregation interval [min].
## % bucket    = Rain gauge bucket volume [mm].
## % mintip    = Aggregate at 1-min before interpolation [default: true].
## % halves    = Add zero rates at estimated event endpoints [default: true].
## % flag1     = leave empty NOT to graph data inventory and event plots.
aggregation_cs <- function(Event_Date, Event_mm, scale, bucket, mintip, halves, ...) {
  ## %iMHEA Cubic spline interpolation rainfall aggregation.
  ##
  ## Input:
  ## ======
  ## Event_Date= dd/mm/yyyy hh:mm:ss [date format].
  ## Event_mm  = Precipitation [mm].
  ## scale     = Agregation interval [min].
  ## bucket    = Rain gauge bucket volume [mm].
  ## mintip    = Aggregate at 1-min before interpolation [default: true].
  ## halves    = Add zero rates at estimated event endpoints [default: true].
  ## flag1     = leave empty NOT to graph data inventory and event plots.
  ##
  ## Return:
  ## =======
  ## NewDate   = dd/mm/yyyy hh:mm:ss [date format] at specified interval.
  ## NewP      = Agregated precipitation [mm].
  ## CumP      = Cumulative rainfall [mm].
  ## Single    = Single tip rainfall rates [mm].

  ## FOR TESTING:
  library(tidyverse)
  library(lubridate)
  file = "inst/extdata/iMHEA_raw/HUA/iMHEA_HUA_01_PD_01_raw.csv"
  x <- readr::read_csv(file)
  tz <- "Etc/GMT-5"
  try(tz <- lutz::tz_lookup_coords(lat, lon, method = "accurate"), silent = TRUE)
  options(digits.secs = 6)
  Event_Date <- x[["Date"]] %>% as.POSIXct(tz = tz, format = "%d/%m/%Y %H:%M:%OS")
  Event_mm <- x[["Event mm"]]
  Event_mm <- remove_consecutive_tips(Event_mm, Event_Date)

  ## %% USER-DEFINED VARIABLES
  ## % Minimum intensity to separate events: 0.2 mm h^-1 [Padron et al, 2015].
  Minint = 0.2/1; # % 0.2 mm over 1 hour
  ## % Maximum intensity to merge events: 12.7 cm h^-1 [Onset, 2013],
  ## % or 300 mm h^-1 [Manz, personal communication].
  Maxint = 127;
  ## % Intensity to distribute single tips: 3 mm h^{-1} [Wang et al, 2008].
  Meanint = 3;
  ## % Threshold intensity above which data is kept:
  ## % 0.10 mm h^{-1} [Wang et al, 2008] or 1/2 Minint.
  Lowint = min(0.1/60,Minint/120); # % [mm min^{-1}]
  ## % Move date by 0.25 seconds to avoid numerical or exportation errors.
  Event_Date = Event_Date - seconds(0.25) #0.25/86400;
  ## % Event_Date = Event_Date + 0.25/86400;
  ## % Add zero rates at estimated initial and final event times, using (Vb/2):
  ## % 1 half rate and 1 half partial tip [Sadler and Busscher, 1989].
  ## % halves = true; % [default: true]

  ## %% INITIALISE VARIABLES
  ## fprintf('\n')
  ## fprintf('RAINFALL AGGREGATION USING CUBIC SPLINE INTERPOLATION.\n')
  scale = 1
  bucket = 0.2
  ## Event_mm = bucket * rep(1, length(Event_Date))
  mintip = TRUE
  halves = TRUE
  ## if nargin < 2 || isempty(Event_mm)
  ##     % Bucket volume in the rain gauge assumed to be 0.2 if not given.
  ##     Event_mm = 0.2*ones(size(Event_Date));
  ## end
  ## if nargin < 3 || isempty(scale)
  ##     % Aggregation time interval assumed to be 1 min
  ##     scale = 1;
  ## end
  ## if nargin < 4 || isempty(bucket)
  ##     % Bucket volume in the rain gauge assumed to be 0.2 if not given.
  ##     bucket = 0.2;
  ## end
  ## if nargin < 5 || isempty(mintip)
  ##     % Do not aggregate at 1-min before interpolation.
  ##     mintip = true;
  ## end
  ## if nargin < 6 || isempty(halves)
  ##     % Add zero rates at estimated event endpoints.
  ##     halves = true;
  ## end
  ## % IT IS RECOMMENDED TO DEPURE UNREALISTIC EVENTS BEFORE RUNNING THE CODE
  ## % Unrealistic events are those which happen consecutively after 1 second.
  ## % [Event_mm] = iMHEA_Depure(Event_Date,Event_mm);

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

  ## %% TRANFORM DATES TO NUMBERS FOR EASIER PROCESSING - TODO check if necessary
  ## % Modified variables to process.
  ## Event_Date = Event_Date # datenum(Event_Date)
  NewEvent_Date = Event_Date
  NewEvent_mm = Event_mm
  NewEvent_Date = NewEvent_Date[!is.na(NewEvent_mm)]
  NewEvent_mm = NewEvent_mm[!is.na(NewEvent_mm)]

  ## %% PREPROCESS RAINFALL EVENTS
  nd = 1440; # % Number of minutes per day or numeric value of 1 minute: 1/1440
  ## Maximum tip interval to separate events.
  ## N.B.
  ## * [bucket / Minint] gives the minimum time between tips in hours
  ## * multiplying by [60 * (1 / nd)] converts to days
  ## * we actually want the units in seconds, so instead of multiplying by [60 * (1 / nd)], we multiply by 3600
  ## MaxT = 60 * (1 / nd) * bucket / Minint;
  ## ## % Minimum tip interval to merge events.
  ## MinT = 60 * (1 / nd) * bucket / Maxint;
  MaxT = 60 * 60 * (bucket / Minint)
  ## % Minimum tip interval to merge events.
  MinT = 60 * 60 * (bucket / Maxint)
  ## % Aggregate events to avoid large intensities
  if (mintip) {
    ## Aggregate data at 1-min scale.
    x = aggregate_events(NewEvent_Date, NewEvent_mm)
  } else {
    ## % Merge rainfall tips occurring at extremely short periods.
    x = merge_events(NewEvent_Date, NewEvent_mm, MinT)
  }
  ## TODO work with dataframes throughout
  NewEvent_Date = x$Date; NewEvent_mm = x$Prec
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
  NewEvent_Date = x$Date; NewEvent_mm = x$Prec
  ## [NewEvent_Date,NewEvent_mm] = DivideEvents(NewEvent_Date,NewEvent_mm,MaxT);
  ## % Redistribute rainfall over relatively long periods slightly shorter.
  ## % [NewEvent_Date,NewEvent_mm] = DivideEvents(NewEvent_Date,NewEvent_mm,MaxT/2);
  ## % Remove initial extreme to avoid crashing the code later.
  ## NewEvent_Date(1) = [];
  ## NewEvent_mm(1) = [];
  NewEvent_Date = NewEvent_Date[2:length(NewEvent_Date)]
  NewEvent_mm = NewEvent_mm[2:length(NewEvent_mm)]
  ## if nargin > 6
  ##     % Plot the new half tips
  ##     plot(datetime(NewEvent_Date(NewEvent_mm~=bucket),'ConvertFrom','datenum'),...
  ##         NewEvent_mm(NewEvent_mm~=bucket),'o','DisplayName','Auxiliary events')
  ##     plot(datetime(Event_Date(Event_mm==0),'ConvertFrom','datenum'),...
  ##         Event_mm(Event_mm==0),'xk','DisplayName','Zero intensity')
  ##     set(gca,'Ylim',[-2 max(NewEvent_mm)+1])
  ##     legend('off'); legend('show')
  ##     legend('location','SouthWest'); legend('boxoff')
  ## end

  ## Identify events from tips separated by more than the maximum time MaxT.
  NewEventDiff = int_length(int_diff(NewEvent_Date)) > MaxT
  NewEventDiff = c(TRUE, NewEventDiff) # Make first point the start of a new event
  ## `indx` represents the index of each new event
  indx = which(NewEventDiff)
  ## Number of elements per event
  ## [assumption that events lasting longer than MaxT are in fact a separate event?]
  n = diff(indx) - 1
  n_last = length(NewEvent_Date) - rev(indx)[1]
  n = c(n, n_last)
  ## Duration of the events in seconds
  n_indx = length(indx)
  D = (NewEvent_Date[indx[2:n_indx] - 1] - NewEvent_Date[indx[1:(n_indx - 1)]]) #* 1440
  D_last = rev(NewEvent_Date)[1] - NewEvent_Date[indx[n_indx]]
  D = c(D, D_last)
  n1 = indx[n < 1] # Index of events with only 1 point
  ## fprintf('Number of rainfall events identified: %6i.\n',length(indx))
  ## fprintf('Average duration of the events: %8.2f min.\n',mean(D(D>0)))
  ## fprintf('Rainfall events consisting of 1 tip only: %6i.\n',length(n1))

  ## FIT EVENTS AND AGGREGATING AT 1-min INTERVAL
  ## Build a 1 minute cumulative rainfall curve
  DI = floor_date(min(Event_Date), unit = "minute")
  DF = ceiling_date(max(Event_Date), unit = "minute")
  NewDate_1min = seq(DI, DF, by = "1 min")
  CumP_1min = rep(0, length(NewDate_1min))   # Initialise accumulation
  Single_1min = rep(0, length(NewDate_1min)) # Initialise single tip counting
  biased = rep(0, length(n))                 # Initialise bias vector
  bEvent = rep(0, length(n))                 # Initialise biased events counter

  for (i in 1:length(n)) {
    ## Procedure:
    ## % Events with more than 2 points (fit a CS) [Wang et al, 2008].
    ## % Events with only 2 points (fit a line) [Ciach, 2003].
    ## % Events with only 1 points (distribute at a rate of 3 mm h^{-1}) [Wang et al, 2008].
    if (n[i] >= 1) {
      ## Relative time in seconds from the beginning of the event.
      x = (NewEvent_Date[indx[i] + (0:n[i])] - NewEvent_Date[indx[i]])
      ## Cumulative rainfall during the event
      y = cumsum(NewEvent_mm[indx[i]:(indx[i] + n[i])])
      if (halves) {
        ## Estimate initial point of the rainfall event
        ## Reduce half a second only to ensure correct initial data calculation
        x0 = bucket * (x[2] - x[1]) / (y[2] - y[1]) - 0.5
        xf = bucket * (rev(x)[1] - rev(x)[2]) / (rev(y)[1] - rev(y)[2])
        ## Allocate only 1-half tip at the start and end of event
        x = x + x0
        y = y - bucket / 2
        y = c(0, y, rev(y)[1] + bucket / 2)
        x = c(0, x, rev(x)[1] + xf)
        x = round(x) # TODO check - would floor/ceiling be better?

        ## Aggregating data at 1-min interval starting at :00
        DI = max(DI, floor_date(NewEvent_Date[indx[i]] - x0, unit = "minute"))
        DF = ceiling_date(NewEvent_Date[indx[i] + n[i]] + xf)
        x1m = seq(DI, DF, by = "1 min") - NewEvent_Date[indx[i]] + x0 # Equally spaced time interval
        ## x1m = round(60 * x1m) # Convert to seconds
      } else {
        ## x0 = -0.5
        ## Aggregating data at 1-min interval starting at :00
        DI = max(DI, floor_date(NewEvent_Date[indx[i]] + seconds(0.5)))
        DF = ceiling_date(NewEvent_Date[indx[i] + n[i]])
        x1m = seq(DI, DF, by = "1 min") - NewEvent_Date[indx[i]]
        ## x1m = round(60 * x1m) # Convert to seconds
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
      if (halves) {
        ## % Set the estimated zero rate endpoints first derivatives to 0
        ## % [Sadler and Busscher, 1989].
        ## pp = spline(x, c(0, y, 0)) # TODO check this
        y1m = pracma::cubicspline(x, y, xi=as.numeric(x1m), endp2nd = TRUE, der = c(0, 0))
      } else {
        ## % Set the endpoints second derivatives to 0 [Wang et al, 2008].
        ## N.B. a natural cubic spline is a cubic spline that sets second
        ## derivatives to zero at the end points.
        ## E.g. https://stats.stackexchange.com/q/322047
        ## pp = csape(x, y, 'second') # TODO
        ## y1m = spline(x, y, method = 'hyman', xout = x1m)
        y1m = spline(x, y, method = 'natural', xout = x1m)
      }
      ## ## The above spline routines work as in the MATLAB original,
      ## ## but can result in negative rainfall intensities. Using a
      ## ## monotonic spline prevents this:
      ## y1m = spline(c(x[1] - 60, x, rev(x)[1] + 60), c(y[1], y, rev(y)[1]), method = 'hyman', xout = x1m)

      ## y1m = fnval(pp,x1m); # % Cumulative rainfall at each x1m TODO
      if (halves) {
        ## % Zero rainfall rates at borders.
        y1m[1] = 0
        y1m[length(y1m)] = y1m[length(y1m) - 1]
      }
      ## r1m = [y1m(1);diff(y1m)]; % Rainfall rate at each x1m [mm min^{-1}]
      r1m = compute_rainfall_intensity(y1m) # Rainfall rate at each x1m [mm min^{-1}]
      ## % Correction for negative intensities and biased volumes.
      ## TODO intCorrection(...)
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
      ## [r2m,y2m,biased(i),bEvent(i)] = intCorrection(r1m,y,Lowint,halves,x,x1m);
      ## Assemble cumulative rainfall curve
      ix = NewDate_1min >= DI & NewDate_1min <= DF
      CumP_1min[ix] = CumP_1min[ix] + y2m
      CumP_1min[NewDate_1min > DF] = CumP_1min[NewDate_1min == DF]
      stop()
      ## TODO [only needed for plotting]
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
      ## Aggregating data at 1-min interval starting at :00
      x0 = NewEvent_mm[indx[i]] / Meanint * 60 - 1 # Time interval in [min]
      xf = NewEvent_Date[indx[i]] * nd             # Final date in [min]
      x = (xf - x0 * nd / 1440:xf)                 # Equally spaced divided tip
      DI = floor((xf - x0 * nd / 1440))            # Initial date in [min]
      DF = ceiling(xf)                             # Final date in [min]
      x1m = (DI:DF)                                # Equally spaced time interval
      y = NewEvent_mm[indx[i]] * rep(1, length(x)) / (x0 + 1)
      ## Tip counting
      r1m = rep(0, length(x1m))
      if (x[1] == x1m[1]) {
        j = 2 # Data counter
        r1m[1] = y[1]
      } else {
        j = 1 # Data counter
      }
      for (itb in 2:length(x1m)) {
        ## Aggregate values
        while (j <= length(y) & x[j] > x1m[itb-1] & x[j] <= x1m[itb]) {
          r1m[itb] = r1m[itb] + y[j]
          j = j + 1
        }
      }
      y1m = cumsum(r1m) # Cumulative rainfall at each x1m
      ## Assemble cumulative rainfall curve
      CumP_1min[NewDate_1min >= DI & NewDate_1min <= DF] = CumP_1min[NewDate_1min >= DI & NewDate_1min <= DF] + y1m
      CumP_1min[NewDate_1min > DF] = CumP_1min[NewDate_1min == DF]
      ## Assemble single tip rainfall vector
      Single_1min[NewDate_1min >= DI & NewDate_1min <= DF] = Single_1min[NewDate_1min >= DI & NewDate_1min <= DF] + y1m
      Single_1min[NewDate_1min > DF] = Single_1min[NewDate_1min == DF]
    }
  }
  ## fprintf('Maximum bias corrected in event interpolation: %5.2f%%.\n',100*nanmax(biased))
  ## fprintf('%2i event(s) with bias >25%% interpolated linearly.\n',nansum(bEvent))
  ## fprintf('\n')
  ## % drawnow
  ## % delete(findall(0,'Type','figure'))

  ## %% PREPARE THE DATA VECTORS AT THE SPECIFIED SCALE

  ## % Equally spaced time interval
  NewDate = seq(NewDate_1min[1], NewDate_1min[length(NewDate_1min)], by = scale) # CHECK
  ## NewDate = (NewDate_1min(1):scale:NewDate_1min(end))';
  if (halves) {
    ## Rainfall rate at scale interval obtained from fitted cumulative rainfall
    NewP = c(bucket / 2, CumP_1min[seq(scale + 1, length(CumP_1min), by = scale)] - CumP_1min[seq(1, length(CumP_1min) - scale, scale)])
    ## % Aggregate single tip counting at scale interval.
    Single = c(bucket / 2, Single_1min[seq(scale + 1, length(Single_1min), by = scale)] - Single_1min[seq(1, length(Single_1min) - scale, scale)])
  } else {
    ## Rainfall rate at scale interval obtained from fitted cumulative rainfall
    NewP = c(0, CumP_1min[seq(scale + 1, length(CumP_1min), by = scale)] - CumP_1min[seq(1, length(CumP_1min) - scale, scale)])
    ## % Aggregate single tip counting at scale interval.
    Single = c(0, Single_1min[seq(scale + 1, length(Single_1min), by = scale)] - Single_1min[seq(1, length(Single_1min) - scale, scale)])
  }
  ## if halves ~= false || halves ~= 0
  ##     % Rainfall rate at scale interval obtained from fitted cumulative rainfall.
  ##     NewP = [bucket/2;CumP_1min(scale+1:scale:end) - CumP_1min(1:scale:end-scale)];
  ##     % Aggregate single tip counting at scale interval.
  ##     Single = [bucket/2;Single_1min(scale+1:scale:end) - Single_1min(1:scale:end-scale)];
  ## else
  ##     % Rainfall rate at scale interval obtained from fitted cumulative rainfall.
  ##     NewP = [0;CumP_1min(scale+1:scale:end) - CumP_1min(1:scale:end-scale)];
  ##     % Aggregate single tip counting at scale interval.
  ##     Single = [0;Single_1min(scale+1:scale:end) - Single_1min(1:scale:end-scale)];
  ## end
  ## % Cumulative rainfall at scale interval.
  CumP = cumsum(NewP)
  ## % Correct numerical errors in the calculations of zero rain rates.
  NewP[round(NewP, 8) == 0] = 0
  Single[round(Single,8) == 0] = 0
  ## % Cut the vectors to the actual initial and final date.
  nd = 1440 / scale # % Number of intervals per day
  DI = ceiling(min(Event_Date) * nd) * scale # % Initial date in [min]
  DF = ceiling(max(Event_Date) * nd) * scale # % Final date in [min]
  CumP[NewDate < DI | NewDate > DF] = NA
  NewP[NewDate < DI | NewDate > DF] = NA
  Single[NewDate < DI | NewDate > DF] = NA
  NewDate[NewDate < DI | NewDate > DF] = NA
  NewDate = NewDate / 1440 # % Rescale the date
  ## % Example (or validation) for 1 minute aggregation.
  ## % NewDate = NewDate_1min/1440;
  ## % CumP = CumP_1min;
  ## % NewP = [CumP(1);diff(CumP)];

  ## TODO - how we do this section depends a lot on the above implementation
  ## % Restoring dates to date format
  ## NewDate = datetime(NewDate,'ConvertFrom','datenum');
  ## ## % Placing data gaps again in the aggregated vectors.
  ## for i = 1:size(Voids,1)
  ##     CumP(NewDate>Voids(i,1) & NewDate<Voids(i,2)) = NaN;
  ##     NewP(NewDate>Voids(i,1) & NewDate<Voids(i,2)) = NaN;
  ##     Single(NewDate>Voids(i,1) & NewDate<Voids(i,2)) = NaN;
  ## end
  ## if nargout == 1
  ##     NewDate = [datenum(NewDate),NewP,CumP,Single];
  ## end
  ## fprintf('Rainfall volume before aggregation: %8.2f mm.\n',nansum(Event_mm))
  ## fprintf('Rainfall volume after aggregation: %8.2f mm.\n',nansum(NewP))
  ## fprintf('\n')
}


aggregate_events <- function(Event_Date, Event_mm) {
  ## Agregate rainfall at 1-min intervals.
  ## DI = floor_date(min(Event_Date), unit = "minute")
  ## DF = ceiling_date(max(Event_Date), unit = "minute")
  ## NewDate_1min = seq(DI, DF, by = "1 min")
  x =
    tibble(Date = Event_Date, Prec = Event_mm) %>%
    mutate(Date = floor_date(Event_Date, unit = "minute")) %>%
    group_by(Date) %>%
    summarise(Prec = sum(Prec))
  x
  ## ## DI = floor(min(Event_Date))*nd; # % Initial date [day]
  ## ## DF = ceil(max(Event_Date))*nd; # % Final date [day]
  ## ## NewDate_1min = seq(DI, DF, by=1)
  ## ## NewDate_1min = (DI:DF)'; % Equally spaced time interval
  ## n = length(NewDate_1min); # % Number of 1-min intervals
  ## NewP_1min = rep(0, length(NewDate_1min))
  ## ## ## NewP_1min = zeros(size(NewDate_1min)); # % Initialise aggregation
  ## ## if (nd * Event_Date[1] == NewDate_1min[1]) {
  ## ##   j = 2 # Data counter
  ## ##   NewP_1min[1] = Event_mm[1]
  ## ## } else {
  ## ##   j = 1 # Data counter
  ## ## }
  ## for (i in 2:n) {
  ##   ## Aggregate values
  ##   while (j <= k & nd * Event_Date[j] <= NewDate_1min[i]) {
  ##     NewP_1min[i] = NewP_1min[i] + Event_mm[j]
  ##     j = j+1
  ##   }
  ## }
  ## ## if nd*Event_Date(1)==NewDate_1min(1)
  ## ##     j = 2; % Data counter
  ## ##     NewP_1min(1) = Event_mm(1);
  ## ## else
  ## ##     j = 1; % Data counter
  ## ## end
  ## ## for i = 2:n
  ## ##     % Aggregate values.
  ## ##     while j<=k && nd*Event_Date(j)<=NewDate_1min(i) % && nd*Event_Date(j)>NewDate_1min(i-1)
  ## ##         NewP_1min(i) = NewP_1min(i) + Event_mm(j);
  ## ##         j = j+1;
  ## ##     end
  ## ## end
  ## NewDate_1min = NewDate_1min/nd; # % Rescale the date
  ## ## % Delete zero events to help process relevant data only.
  ## NewDate_1min = NewDate_1min[!NewDate_1min == 0]
  ## NewP_1min = NewP_1min[!NewP_1min == 0]
  ## ## NewDate_1min(NewP_1min==0) = [];
  ## ## NewP_1min(NewP_1min==0) = [];
  ## fprintf('Routine for aggregating tips at 1-min time interval.\n')
  ## fprintf('New number of data points: %4i.\n',length(NewP_1min))
  ## fprintf('Rainfall volume before aggregation: %8.2f mm.\n',nansum(Event_mm))
  ## fprintf('Rainfall volume after aggregation: %8.2f mm.\n',nansum(NewP_1min))
  ## fprintf('\n')
}

merge_events <- function(Event_Date, Event_mm, MinT) {
  ## Delete tips for small periods.
  x =
    tibble(Date = Event_Date, Prec = Event_mm) %>%
    mutate(
      interval = c(int_length(int_diff(Date)), MinT * 100),
      event_count = seq(1, n())
    ) %>%
    mutate(event_count = ifelse(interval < MinT, lead(event_count), event_count)) #%>%
  x_merged =
    x %>%
    group_by(event_count) %>%
    summarize(Prec = sum(Prec)) %>%
    left_join(
      x %>% dplyr::select(Date, event_count),
      by = "event_count"
    ) %>%
    dplyr::select(Date, Prec, -event_count)
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
  ## Event_Date = xx
  ## Event_mm = yy
  ## Event_Date = NewEvent_Date
  ## Event_mm = NewEvent_mm
  diff_event_date = int_length(int_diff(Event_Date))
  event_diff = diff_event_date > MaxT
  half_event_diff = diff_event_date > MaxT / 2
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
      t0 = Event_Date[i] - diff_event_date[i-1] / 2
      ## Include these data in the rainfall tip time series
      NewEvent_Date = c(NewEvent_Date[1:(j-1)], t0, NewEvent_Date[j:length(NewEvent_Date)])
      NewEvent_mm = c(NewEvent_mm[1:(j-1)], halftip_mm, halftip_mm, NewEvent_mm[(j+1):length(NewEvent_mm)])
      j = j + 1
    }
  }
  ## fprintf('Routine for spreading tips occurring between %6.2f and %6.2f minutes.\n',MaxT*1440/2,MaxT*1440)
  ## fprintf('Number of tips added: %4i.\n',j-i)
  ## fprintf('Rainfall volume before spreading: %8.2f mm.\n',nansum(Event_mm))
  ## fprintf('Rainfall volume after spreading: %8.2f mm.\n',nansum(NewEvent_mm))
  ## fprintf('\n')
  tibble(Date = NewEvent_Date, Prec = NewEvent_mm)
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
