## Author : Simon Moulds
## Date   : April 2022

## The purpose of this test is to compare the functions
## with the original Matlab functions.

## We will follow the iMHEA_Workflow.m workflow,
## comparing outputs as we go

library(devtools)
library(imhea)
library(tidyverse)

iMHEA_Catchment_AREA = read_csv(
  "inst/extdata/iMHEA_indices/iMHEA_Data_Areas.csv",
  show_col_types = FALSE
)

## TODO add this to package
valid_timezones <- function() return(OlsonNames())
imhea_to_tsibble <- function(x,
                             date_column = "Date",
                             date_format = "%d/%m/%Y %H:%M:%S",
                             tz = "Etc/GMT-5") {
  stopifnot(date_column %in% names(x))
  stopifnot(tz %in% valid_timezones())
  x =
    x %>%
    rename(Date = date_column) %>%
    mutate(Date = as.POSIXct(Date, tz = tz, format = date_format)) %>%
    rowid_to_column() %>%
    as_tsibble(key = rowid, index = Date, regular = FALSE)
  x
}

tipping_bucket_rain_gauge <- function(x,
                                      date_column = "Date",
                                      date_format = "%d/%m/%Y %H:%M:%S",
                                      tz = "Etc/GMT-5",
                                      event_column = "Event mm",
                                      flag_column = "Flag",
                                      raw = TRUE,
                                      ...) {

  stopifnot(date_column %in% names(x))
  stopifnot(event_column %in% names(x))
  x = x %>% rename(Date = date_column, Event = event_column)
  x = x %>% imhea_to_tsibble(date_column, ...)
  if (!raw) {
    class(x) <- c("tipping_bucket_rain_gauge", class(x))
    return(x)
  }
  ## TODO
  class(x) <- c("tipping_bucket_rain_gauge", class(x))
  return(x)
}

## LLO_01
## iMHEA_LLO_01_01_HI_01_raw =
q1 = read_csv(
  "inst/extdata/iMHEA_raw/LLO/iMHEA_LLO_01_HI_01_raw.csv",
  show_col_types = FALSE
)

## iMHEA_LLO_01_PO_01_raw =
p1 = read_csv(
  "inst/extdata/iMHEA_raw/LLO/iMHEA_LLO_01_PO_01_raw.csv",
  show_col_types = FALSE
)

## iMHEA_LLO_01_PO_02_raw =
p2 = read_csv(
  "inst/extdata/iMHEA_raw/LLO/iMHEA_LLO_01_PO_02_raw.csv",
  show_col_types = FALSE
)

## Do some initial data preparation
p1 <- p1 %>% tipping_bucket_rain_gauge() %>% depure()
p2 <- p2 %>% tipping_bucket_rain_gauge() %>% depure()

## TODO set verbosity of package

## iMHEA_Depure comparison
matlab_output_p1 = read_csv("inst/testdata/matlab_depure_output_llo_p1.csv")
matlab_output_p2 = read_csv("inst/testdata/matlab_depure_output_llo_p2.csv")
