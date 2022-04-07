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
## TODO helper function to convert many objects to tsibble objects
tz <- "Etc/GMT-5"
p1 <-
  p1 %>%
  mutate(Date = as.POSIXct(Date, tz = tz, format = "%d/%m/%Y %H:%M:%S")) %>%
  mutate(key = seq(1, nrow(p1)))
p1 <- p1 %>% as_tsibble(key = key, index = Date, regular = FALSE)

## iMHEA_Depure comparison
matlab_output_p1 = read_csv("inst/testdata/matlab_depure_output_llo_p1.csv")
matlab_output_p2 = read_csv("inst/testdata/matlab_depure_output_llo_p2.csv")
