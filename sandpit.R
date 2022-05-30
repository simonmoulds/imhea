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
  x =
    x %>%
    rename(Date = date_column) %>%
    mutate(Date = as.POSIXct(Date, tz = tz, format = date_format)) %>%
    rowid_to_column() %>%
    as_tsibble(key = rowid, index = Date, regular = regular)
  x
}

## S3 class
tipping_bucket_rain_gauge <- function(x,
                                      date_column = "Date",
                                      date_format = "%d/%m/%Y %H:%M:%S",
                                      tz = "Etc/GMT-5",
                                      event_column = "Event mm",
                                      event_units,
                                      flag_column = "Flag",
                                      raw = TRUE,
                                      ...) {

  stopifnot(date_column %in% names(x))
  stopifnot(event_column %in% names(x))
  stopifnot(!missing(event_units))

  x <- x %>% rename(Date = date_column, Event = event_column)
  x <- x %>% imhea_to_tsibble(date_column, ..., regular = FALSE)
  x <- x %>% mutate(Event = set_units(Event, event_units, mode = "standard"))
  if (!raw) {
    class(x) <- c("tipping_bucket_rain_gauge", class(x))
    return(x)
  }
  ## TODO
  class(x) <- c("tipping_bucket_rain_gauge", class(x))
  return(x)
}

## S3 class
stream_gauge <- function(x,
                         date_column = "Date",
                         discharge_column = "Flow l/s",
                         discharge_units,
                         level_column = NA,
                         level_units,
                         flag_column = "Flag",
                         raw = TRUE,
                         ...) {

  stopifnot(date_column %in% names(x))
  stopifnot(discharge_column %in% names(x))
  stopifnot(is.na(level_column) | isTRUE(level_column %in% names(x)))
  x <- x %>% rename(Date = date_column, Q = discharge_column, Flag = flag_column)
  if (is.na(level_column)) {
    x <- x %>% dplyr::select(Date, Q, Flag)
  } else {
    x <- x %>% rename(H = level_column) %>% dplyr::select(Date, Q, H, Flag)
  }
  x <- x %>% imhea_to_tsibble(date_column, ..., regular = FALSE)
  x <- x %>% mutate(Q = set_units(Q, discharge_units, mode = "standard"))
  if ("H" %in% names(x))
    x <- x %>% mutate(H = set_units(H, level_units, mode = "standard"))
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
p1 <-
  p1_raw %>%
  tipping_bucket_rain_gauge(event_units = "mm") %>%
  depure() # TODO depure should not return p1 with a new column 'Interval'

p2 <-
  p2_raw %>%
  tipping_bucket_rain_gauge(event_units = "mm") %>%
  depure()

q1 <-
  q1_raw %>%
  stream_gauge(
    discharge_units = "m^3/s",
    level_column = "Level cm",
    level_units = "cm"
  ) ## %>%
  ## tsibble::fill_gaps(.full = TRUE)

## Aggregate precipitation to match discharge interval

## Determine discharge interval
## Note int_length(int_diff(...)) always returns seconds
int_HRes <- median(int_length(int_diff(q1[[index(q1)]])))
## Number of intervals per day
## nd <- 86400 / int_HRes
## p1 <- p1 %>% aggregation_cs() # TODO
## p2 <- p2 %>% aggregation_cs() # TODO

## Test some of the functions called within aggregation_cs(...)
## Setup [from aggregation_cs(...)]
x <- p1
timescale <- set_units(60 * 5, "s")
## timescale <- set_units(60, "s")
bucket <- set_units(0.2, "mm")
mintip <- TRUE
halves <- TRUE

## This works, but using Matlab output directly for testing
## x1 <- aggregation_cs(p1, timescale = timescale)
## ## Not working properly:
## ## Problem arises because of negative time differences
## p2$Date %>% diff() %>% min()
## p2 <- p2 %>% arrange(Date)
## p2$Date %>% diff() %>% min()
## x2 <- aggregation_cs(p2, timescale = timescale)

x1 <-
  read_csv("inst/testdata/matlab_aggregation_cs_output_llo_p1.csv") %>%
  setNames(c("Date", "NewP", "CumP", "Single")) %>%
  mutate(Date = (Date - 719529) * 86400) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
  mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
  mutate(Date = force_tz(Date, "Etc/GMT-5"))

x2 <-
  read_csv("inst/testdata/matlab_aggregation_cs_output_llo_p2.csv") %>%
  setNames(c("Date", "NewP", "CumP", "Single")) %>%
  mutate(Date = (Date - 719529) * 86400) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
  mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
  mutate(Date = force_tz(Date, "Etc/GMT-5"))

Date1 <- x1$Date
P1 <- x1$NewP
Date2 <- x2$Date
P2 <- x2$NewP

## This works (at least the parts that I've tested)
x_fill <- fill_gaps(x1$Date, x1$NewP, x2$Date, x2$NewP)

x_matlab_fill_gaps_output <-
  read_csv("inst/testdata/matlab_fill_gaps_output_llo_p1.csv") %>%
  setNames(c("Date", "P1", "P2")) %>%
  mutate(Date = (Date - 719529) * 86400) %>%
  mutate(Date = as.POSIXct(Date, tz = "UTC", origin = "1970-01-01")) %>%
  mutate(Date = round_date(Date, unit = "0.25 seconds")) %>%
  mutate(Date = force_tz(Date, "Etc/GMT-5"))

PrecHRes <- list(x1, x2)
nrg <- 2
if (nrg > 1) {
  ## Fill precipitation gaps between all combinations of rain gauges
  combinations <- combn(1:nrg, 2)
  combn_index <- dim(combinations)[2]
  PrecHResFill <- list()
  for (i in 1:combn_index) {
    a <- x1
    b <- x2
    PrecHResFill[[i]] <-
      fill_gaps(a$Date, a$NewP, b$Date, b$NewP) %>%
      setNames(c("Date", paste0("P1_", i), paste0("P2_", i)))
  }
  myfun <- function(x, y, ...) full_join(x, y, by = "Date")
  Precp_Fill_Compiled <-
    Reduce(myfun, PrecHResFill) %>%
    gather(-Date, key = "key", value = "value") %>%
    group_by(Date) %>%
    summarize(P_HRes = mean(value, na.rm = TRUE))
  DateP_HRes <- Precp_Fill_Compiled$Date
  P_HRes <- Precp_Fill_Compiled$P_HRes
} else {
  DateP_HRes <- PrecHRes[[1]]$Date
  P_HRes <- PrecHRes[[1]]$NewP
}

q1 <- average(q1$Date, q1$Q, timescale)

x <- q1 %>% full_join(x_fill)

x_daily <-
  x %>%
  as_tibble() %>%
  mutate(Date = ceiling_date(Date, unit = "1 day")) %>%
  group_by(Date) %>%
  summarize(n = sum(is.na(Q)), Q = mean(Q), across(starts_with("Prec"), mean, na.rm = TRUE))

x_hourly <-
  x %>%
  as_tibble() %>%
  mutate(Date = ceiling_date(Date, unit = "1 hour")) %>%
  group_by(Date) %>%
  summarize(n = sum(is.na(Q)), Q = mean(Q), across(starts_with("Prec"), sum, na.rm = TRUE))

stop()

## Baseflow


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
