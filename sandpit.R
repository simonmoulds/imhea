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
  x <- x %>% imhea_to_tsibble(date_column, ..., regular = TRUE)
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
nd <- 86400 / int_HRes
p1 <- p1 %>% aggregation_cs() # TODO
p2 <- p2 %>% aggregation_cs() # TODO

## Test voids function [this is usually called within aggregation function]
Voids = identify_voids(Event_Date, Event_mm)

## Original MATLAB version
## %% AGGREGATE PRECIPITATION DATA TO MATCH DISCHARGE INTERVAL
## % Determine discharge interval
## int_HRes = diff(datenum(DateQ))*1440;
## int_HRes = round(nanmedian(int_HRes)); % Worst discharge interval defines the max resolution
## nd = 1440/int_HRes; % Number of intervals per day
## % Interpolate each rain gauge data at max resolution using the CS algorithm
## PrecHRes = cell(nrg,1);
## for i = 1:nrg
##     [PrecHRes{i}] = iMHEA_AggregationCS(varargin{2*i-1},NewEvent_mm{i},int_HRes,bucket);
## end

stop()


## iMHEA_Depure comparison
matlab_output_p1 = read_csv("inst/testdata/matlab_depure_output_llo_p1.csv")
matlab_output_p2 = read_csv("inst/testdata/matlab_depure_output_llo_p2.csv")
