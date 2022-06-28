## Author : Simon Moulds
## Date   : April 2022

## The purpose of this test is to compare the functions
## with the original Matlab functions.

## We will follow the iMHEA_Workflow.m workflow,
## comparing outputs as we go

## TODO set verbosity of package

library(devtools)
library(tidyverse)
library(tsibble)
library(lubridate)
library(units)

load_all("~/dev/imhea")

iMHEA_Catchment_AREA = read_csv(
  "inst/extdata/iMHEA_indices/iMHEA_Data_Areas.csv",
  show_col_types = FALSE
)

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

## S3 class
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

## S3 class
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

## LLO_01
## iMHEA_LLO_01_01_HI_01_raw =
q1_raw = read_csv(
  "inst/extdata/iMHEA_raw/LLO/iMHEA_LLO_01_HI_01_raw.csv",
  show_col_types = FALSE
)

## iMHEA_LLO_01_PO_01_raw =
p1_raw = read_csv(
  "inst/extdata/iMHEA_raw/LLO/iMHEA_LLO_01_PO_01_raw.csv",
  show_col_types = FALSE
)

## iMHEA_LLO_01_PO_02_raw =
p2_raw = read_csv(
  "inst/extdata/iMHEA_raw/LLO/iMHEA_LLO_01_PO_02_raw.csv",
  show_col_types = FALSE
)

## Do some initial data preparation
## TODO Call aggregation_cs from within constructor?
p1 <- p1_raw %>% tipping_bucket_rain_gauge(id = "LLO_01_P0_01", event_units = "mm")
p2 <- p2_raw %>% tipping_bucket_rain_gauge(id = "LLO_01_P0_02", event_units = "mm")

q1 <-
  q1_raw %>%
  stream_gauge(
    id = "LLO_01_HI_01",
    discharge_units = "l/s",
    level_column = "Level cm",
    level_units = "cm"
  )

## Aggregate precipitation to match discharge interval
## TODO should this be part of a catchment object?

## Determine discharge interval
## Note int_length(int_diff(...)) always returns seconds
int_HRes <- median(int_length(int_diff(q1[[index(q1)]])))
timescale <- set_units(int_HRes, "s")

p1 <- aggregation_cs(p1, timescale = timescale)
p2 <- aggregation_cs(p2, timescale = timescale)
p_merged <- infill_precip(p1, p2, new_id = "LLO_01_P0_merged")

## FIXME - this doesn't work on first run throught
q1 <- aggregate(q1, timescale = timescale)

catchment_id <- "LLO_01"
catchment_area <-
  iMHEA_Catchment_AREA %>%
  filter(Catchment %in% catchment_id) %>%
  `[`(, 2, drop=T)

x <- catchment(
  q1, p1, p2, id = catchment_id,
  ar = set_units(catchment_area, km^2)
)
x_daily <- x %>% aggregate_daily()
x_hourly <- x %>% aggregate_hourly()

## Check tsibble methods:
tsibble::interval(x)
tsibble::interval(x_daily)
tsibble::interval(x_hourly)
tsibble::is_regular(x)

## ## Matlab iMHEA_AggregationCS(...) output
## x1 <-
##   read_csv("inst/testdata/matlab_aggregation_cs_output_llo_p1.csv") %>%
##   setNames(c("Date", "NewP", "CumP", "Single")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## x2 <-
##   read_csv("inst/testdata/matlab_aggregation_cs_output_llo_p2.csv") %>%
##   setNames(c("Date", "NewP", "CumP", "Single")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## ## This works (at least the parts that I've tested)
## x_fill <- fill_gaps(x1$Date, x1$NewP, x2$Date, x2$NewP)

## Testing aggregation functions
## y <- aggregation(p1$Date, as.numeric(p1$NewP), timescale)

## x_matlab_fill_gaps_output <-
##   read_csv("inst/testdata/matlab_fill_gaps_output_llo_p1.csv") %>%
##   setNames(c("Date", "P1", "P2")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## plot(cumsum(x_matlab_fill_gaps_output$P1), type = "l", col = "blue")
## lines(cumsum(x_fill$Prec1), col = "magenta")
## plot(cumsum(x_matlab_fill_gaps_output$P2), type = "l", col = "blue")
## lines(cumsum(x_fill$Prec2), col = "magenta")


## q1 <- average(q1$Date, q1$Q, timescale)
## p_combined <- p_combined %>% rename(P = Event) %>% dplyr::select(-Event)

## ## Check daily aggregation
## x_matlab_daily_streamflow <-
##   read_csv("inst/testdata/matlab_daily_streamflow.csv") %>%
##   setNames(c("Date", "Pm", "Qm", "Qbm")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5")) %>%
##   mutate(Date = as.Date(Date))

## x_matlab_5m_streamflow <-
##   read_csv("inst/testdata/matlab_5m_streamflow.csv") %>%
##   setNames(c("Date", "Qm", "Pm")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## ## These match exactly
## y <- x_daily %>% left_join(x_matlab_daily_streamflow)
## y <- y %>% mutate(Qb = baseflow_uk(Date, Q))

## plot(y$Date, y$Q)
## ## lines(y$Date, y$Qm, col="magenta")
## lines(y$Date, y$Qb, col="blue")
## plot(y$Date, y$Qb)
## lines(y$Date, y$Qbm, col="magenta")

## a <- x_matlab_daily_streamflow %>% na.omit()
## k <- baseflow_recession_constant(a$Date, a$Qbm) # This seems to work
x_matlab_baseflow_input <-
  read_csv("inst/testdata/matlab_baseflow_input.csv") %>%
  mutate(Date = as.Date(Date, format = "%d-%b-%Y"))

## TODO add tidy up baseflow methods

stop()

## Up to this point:
## - what are the main plots needed?
## - what is the main output the user expects?

## Baseflow
Q = x_daily$Q
Date = x_daily$Date

## Indices input
indices_date_input <- read_csv("inst/testdata/matlab_indices_date_input_llo_01.csv")
indices_input <- read_csv("inst/testdata/matlab_indices_input_llo_01.csv")

Date <- indices_date_input[,1,drop=T] %>% as.POSIXct(format = "%d-%b-%Y %H:%M:%S", tz = "Etc/GMT-5")
P <- indices_input[,2,drop=T] %>% as.numeric()
Q <- indices_input[,3,drop=T] %>% as.numeric()
A <- iMHEA_Catchment_AREA[1,2] %>% as.numeric() # km2

## Now we can develop indices scripts using these inputs

## [IndicesP,~,~,IndicesQ,~,~,QYEAR,RRa,RRm,RRl] = iMHEA_Indices(Date,P,Q,A,1);
## % Calculate indices from Olden & Poff (2003).
## [M,F,D,T,R] = iMHEA_IndicesPlus(Date,Q,A,1);
## % Calculate Precipitation climatic indices.
## [ClimateP] = iMHEA_ClimateP(Date,P,1);

## % Calculate indices for Discharge and Precipitation.
## if nargin >= 5
##     [IndicesP,PM,IDC,CumP,DP] = iMHEA_ProcessP(Date,P,1);
##     [IndicesQ,QM,FDC,CumQ,DQ] = iMHEA_ProcessQ(Date,Q,A,1,1);
## else
##     [IndicesP,PM,IDC,CumP,DP] = iMHEA_ProcessP(Date,P);
##     [IndicesQ,QM,FDC,CumQ,DQ] = iMHEA_ProcessQ(Date,Q,A);
## end

## % Runoff Coefficient.
## QYEAR = IndicesQ(8)*365/1000000*86400;
## RRa = QYEAR / IndicesP(1);
## CumQ(:,2) = CumQ(:,2)/1000000*86400;

## if isnan(QYEAR); QYEAR = nanmean(DQ(:,2))/1000000*86400; end
## RRl = nanmean(DQ(:,2))/1000000*86400 / (nanmean(DP(:,2)));

## % Monthly discharge in mm.
## MDays = [31 28 31 30 31 30 31 31 30 31 30 31]';
## QM = QM.*MDays/1000000*86400;
## RRm = nansum(QM)/nansum(PM);

## if nargin >= 5
##     % Transform dates to date format for plots
##     CumPDate = datetime(CumP(:,1),'ConvertFrom','datenum');
##     CumQDate = datetime(CumQ(:,1),'ConvertFrom','datenum');
##     NewDateP = datetime(DP(:,1),'ConvertFrom','datenum');
##     NewDateQ = datetime(DQ(:,1),'ConvertFrom','datenum');

##     figure
##     subplot(2,1,1)
##     bar(Date,P)
##     xlabel('Date')
##     ylabel('Precipitation (mm)')
##     set(gca,'YDir','reverse')
##     Xlim = get(gca,'XLim');
##     title('Input Precipitation')
##     box on

##     subplot(2,1,2)
##     plot(Date,Q/A)
##     xlabel('Date')
##     ylabel('Discharge (l/s/km2)')
##     title('Input Discharge')
##     box on

##     figure
##     subplot(2,1,1)
##     plot(CumPDate,CumP(:,2),CumQDate,CumQ(:,2))
##     title('Cumulative comparison')
##     legend('Cum. Rainfall (mm)','Cum. Discharge(mm)','Location','NorthWest')
##     xlabel('Date')
##     ylabel('Cumulative variables')
##     box on

##     subplot(2,1,2)
##     plot(CumP(:,2),CumQ(:,2))
##     title('Double Mass Plot')
##     xlabel('Precipitation')
##     ylabel('Discharge')
##     box on

##     figure
##     semilogx(IDC(:,1),IDC(:,2))
##     xlabel('Duration (min)')
##     ylabel('Maximum precipitation intensity (mm/h)')
##     title('Maximum Intensity-Duration Curve')
##     box on

##     figure
##     semilogy(FDC(:,1),FDC(:,2))
##     xlabel('Exceedance probability')
##     ylabel('Discharge (l/s/km2)')
##     title('Flow Duration Curve')
##     box on

##     figure
##     plot((1:12)',PM,(1:12)',QM)
##     xlabel('Month')
##     ylabel('Variable (mm)')
##     legend('Precipitation (mm)','Discharge (mm)')
##     title('Monthly Data')
##     xlim([0 13])
##     box on

##     figure
##     subplot(2,1,1)
##     bar(NewDateP,DP(:,2))
##     xlabel('Date')
##     ylabel('Precipitation (mm)')
##     set(gca,'YDir','reverse','XLim',Xlim);
##     title('Daily Precipitation')
##     box on

##     subplot(2,1,2)
##     plot(NewDateQ,DQ(:,2),NewDateQ,DQ(:,3),NewDateQ,DQ(:,4))
##     xlabel('Date')
##     ylabel('Discharge (l/s/km2)')
##     legend('Discharge','Baseflow','Stormflow')
##     set(gca,'XLim',Xlim);
##     title('Daily Discharge')
##     box on
## end

## NOT USED:
##
## avg <- q1 %>% as_tibble() %>%
##   dplyr::select(Date, Q) %>%
##   mutate(NewDate = floor_date(Date, "5 mins")) %>%
##   group_by(NewDate) %>%
##   mutate(Q = as.numeric(Q)) %>%
##   summarise(Q = mean(Q)) %>%
##   rename(Date = NewDate)
## complete_ts <- seq(avg$Date[1], rev(avg$Date)[1], by = "5 mins")
## avg <- avg %>% as_tsibble(index = Date, regular = FALSE)

## ## https://stackoverflow.com/a/35162775
## x <- p1 # TODO remove Key
## x <- x %>% as_tibble()
## v <- x %>% identify_voids()
## ## NOT USED

## x_aggr_matlab_input <-
##   read_csv("matlab_aggregate_events_input.csv", col_names = F) %>%
##   setNames(c("Date", "Prec")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## x_div_matlab_input <-
##   read_csv("matlab_divide_events_input.csv", col_names = F) %>%
##   setNames(c("Date", "Prec")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## x_div_matlab <-
##   read_csv("matlab_divide_events_output.csv", col_names = F) %>%
##   setNames(c("Date", "Prec")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## x_aggr_matlab <-
##   read_csv("inst/testdata/matlab_aggregation_cs_output_llo_p2.csv") %>%
##   setNames(c("Date", "NewP", "CumP", "Single")) %>%
##   mutate(Date = (Date - 719529) * 86400) %>%
##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## plot(x2$Date, x2$CumP, type="l", col="blue")
## lines(x_aggr_matlab$Date, x_aggr_matlab$CumP, col="magenta")

## Event_Date <- x[["Date"]]
## Event_mm <- x[["Event"]]
## Minint = set_units(0.2, "mm/h")
## Maxint = set_units(127, "mm/h")
## Meanint = set_units(3, "mm/h")
## Lowint = min(set_units(0.1, "mm/h"), Minint / 2) # FIXME [mm min^{-1}]

## Event_Date = Event_Date - seconds(0.25)
## ## Test identify_voids(...) function
## Voids = identify_voids(p1) # FIXME - works, but is very slow
## ## ## Event_Date = Event_Date # datenum(Event_Date)
## ## NewEvent_Date = Event_Date
## ## NewEvent_mm = Event_mm
## ## NewEvent_mm[NewEvent_mm == 0] = NA
## ## NewEvent_Date = NewEvent_Date[!is.na(NewEvent_mm)]
## ## NewEvent_mm = NewEvent_mm[!is.na(NewEvent_mm)]

## ## ## Test aggregate_events(...) and merge_events(...)
## ## ## Maximum tip interval to separate events.
## ## ## N.B.
## ## ## * [bucket / Minint] gives the minimum time between tips in hours
## ## ## * multiplying by [60 * (1 / nd)] converts to days
## ## ## * we actually want the units in seconds, so instead of multiplying
## ## ##   by [60 * (1 / nd)], we multiply by 3600
## ## MaxT = set_units(bucket, "mm") / set_units(Minint, "mm/s")
## ## ## Minimum tip interval to merge events.
## ## MinT = set_units(bucket, "mm") / set_units(Maxint, "mm/s")
## ## ## % Aggregate events to avoid large intensities
## ## mintip = TRUE
## ## ## if (mintip) {
## ## ## Aggregate data at 1-min scale [WORKING]
## ## x_aggr <- aggregate_events(NewEvent_Date, NewEvent_mm)
## ## ## } else {
## ## ## Merge rainfall tips occurring at extremely short periods [WORKING, but not tested properly with this dataset]
## ## ## x_aggr = merge_events(NewEvent_Date, NewEvent_mm, MinT)

## ## ## TODO work with dataframes throughout
## ## NewEvent_Date = x_aggr$Date; NewEvent_mm = x_aggr$Prec
## ## ## if mintip == true
## ## ##     % Aggregate data at 1-min scale.
## ## ##     [NewEvent_Date,NewEvent_mm] = AggregateEvents(NewEvent_Date,NewEvent_mm);
## ## ## else
## ## ##     % Merge rainfall tips occurring at extremely short periods.
## ## ##     [NewEvent_Date,NewEvent_mm] = MergeEvents(NewEvent_Date,NewEvent_mm,MinT);
## ## ## end
## ## ## % Adding a supporting initial extreme to avoid crashing the code later.
## ## NewEvent_Date = c(Event_Date[1] - seconds(MaxT), NewEvent_Date)
## ## NewEvent_mm = c(0, NewEvent_mm)
## ## ## NewEvent_Date = cat(1,Event_Date(1)-MaxT,NewEvent_Date);
## ## ## NewEvent_mm = cat(1,0,NewEvent_mm);
## ## ## % Redistribute rainfall tips occurring at relatively long periods.
## ## stop() # divide_events not currently working as expected - 444 tips added instead of 456
## ## length(NewEvent_Date)
## ## length(NewEvent_mm)

## ## ## This is the input to divide_events
## ## y <-
## ##   read_csv("matlab_divide_events_input.csv", col_names = FALSE) %>%
## ##   setNames(c("Date", "Prec")) %>%
## ##   mutate(Date = (Date - 719529) * 86400) %>%
## ##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
## ##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
## ##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## ## x <- tibble(Date = NewEvent_Date, Prec = NewEvent_mm)
## ## all.equal(y, x)

## ## ## NewEvent_mm is equal
## ## ## TODO check dates - remove matlab formatting
## ## x <- divide_events(NewEvent_Date, NewEvent_mm, MaxT)

## ## ## ## I think the cause of the slight discrepancy is a precision error,
## ## ## ## which causes some events separated by exactly 30 minutes (MaxT / 2)
## ## ## ## to be identified as greater than MaxT / 2.

## ## ## ## Test aggregation_cs using these values:
## ## # y <-
## ## #
## ## ##   read_csv("matlab_divide_events_output.csv", col_names = FALSE) %>%
## ## ##   setNames(c("Date", "Prec")) %>%
## ## ##   mutate(Date = (Date - 719529) * 86400) %>%
## ## ##   mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
## ## ##   mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
## ## ##   mutate(Date = force_tz(Date, "Etc/GMT-5"))

## ## ## Event_Date = NewEvent_Date
## ## ## Event_mm = NewEvent_mm

## ## NewEvent_Date = x$Date; NewEvent_mm = x$Prec

## ## stop()

## ## ## d <- data.frame(a = seq(1,20), b = c(runif(3), rep(NA, 4), runif(3), rep(NA, 3), runif(6), rep(NA, 1)))
## ## ## d %>% filter(is.na(b)) %>% mutate(c = c(2, diff(a))) %>% mutate(d = ifelse(c>1,a,b)) %>% mutate(e = ifelse(lead(c)>1, a, b)) %>% mutate(d = na.locf(d, na.rm = T), e = na.locf(e, na.rm = F, fromLast = TRUE))

## ## ## Original MATLAB version
## ## ## %% AGGREGATE PRECIPITATION DATA TO MATCH DISCHARGE INTERVAL
## ## ## % Determine discharge interval
## ## ## int_HRes = diff(datenum(DateQ))*1440;
## ## ## int_HRes = round(nanmedian(int_HRes)); % Worst discharge interval defines the max resolution
## ## ## nd = 1440/int_HRes; % Number of intervals per day
## ## ## % Interpolate each rain gauge data at max resolution using the CS algorithm
## ## ## PrecHRes = cell(nrg,1);
## ## ## for i = 1:nrg
## ## ##     [PrecHRes{i}] = iMHEA_AggregationCS(varargin{2*i-1},NewEvent_mm{i},int_HRes,bucket);
## ## ## end

## ## stop()


## ## ## iMHEA_Depure comparison
## ## matlab_output_p1 = read_csv("inst/testdata/matlab_depure_output_llo_p1.csv")
## ## matlab_output_p2 = read_csv("inst/testdata/matlab_depure_output_llo_p2.csv")
