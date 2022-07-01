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
  system.file("extdata", "iMHEA_Data_Areas.csv", package = "imhea"),
  show_col_types = FALSE
)

q1_raw <- read_csv(
  system.file("extdata", "LLO/iMHEA_LLO_01_HI_01_raw.csv", package = "imhea"),
  show_col_types = FALSE
)

p1_raw = read_csv(
  system.file("extdata", "LLO/iMHEA_LLO_01_PO_01_raw.csv", package = "imhea"),
  show_col_types = FALSE
)

p2_raw = read_csv(
  system.file("extdata", "LLO/iMHEA_LLO_01_PO_02_raw.csv", package = "imhea"),
  show_col_types = FALSE
)

## Convert precipitation data to rain_gauge objects:
p1 <- p1_raw %>% tipping_bucket_rain_gauge(id = "LLO_01_P0_01", event_units = "mm")
p2 <- p2_raw %>% tipping_bucket_rain_gauge(id = "LLO_01_P0_02", event_units = "mm")

## Convert streamflow data to stream_gauge object:
q1 <-
  q1_raw %>%
  stream_gauge(
    id = "LLO_01_HI_01",
    discharge_units = "l/s",
    level_column = "Level cm",
    level_units = "cm"
  )

## Define the catchment ID, and retrieve catchment area:
catchment_id <- "LLO_01"
catchment_area <-
  iMHEA_Catchment_AREA %>%
  filter(Catchment %in% catchment_id) %>%
  `[`(, 2, drop=T)

stop()

## Create a catchment object (this takes a few minutes):
x <- catchment(q1, p1, p2, id = catchment_id, area = set_units(catchment_area, km^2))

## Have a look at the catchment indices:
indices(x)

## Make some plots (we can create some functions to automate this process if needed):
p1 <- ggplot(data = daily(x), aes(x = Date, y = P)) +
  geom_line()

p2 <- ggplot(data = daily(x), aes(x = Date, y = Q)) +
  geom_line()

## Join plots using `patchwork`
library(patchwork)
p1 + p2 + plot_layout(ncol = 1, nrow = 2)

## Plot monthly/annual summaries
p1 <- ggplot(data = monthly(x), aes(x = Date, y = Q)) +
  geom_bar(stat = "identity")

p2 <- ggplot(data = annual(x), aes(x = Date, y = Q)) +
  geom_bar(stat = "identity")

p1 + p2 + plot_layout(ncol = 1, nrow = 2)

## Plot IDC/FDC
p1 <- ggplot(data = idc(x), aes(x = as.numeric(D), y = Intensity)) +
  geom_line() +
  scale_x_continuous(trans = "log10") +
  xlab("Duration [min]")

p2 <- ggplot(data = fdc(x), aes(x = Exceedance_Pct, y = Q)) +
  geom_line() +
  scale_y_continuous(trans = "log10") +
  ylab("Discharge [m3/s]")

## Plot baseflow
d <- baseflow_data(x)[["UKIH"]]
d <- d %>% pivot_longer(cols = starts_with("Q"), names_to = "name", values_to = "values")
p1 <- ggplot(data = d, aes(x = Date, y = values, col = name)) +
  geom_line()

## Monthly climatology with box & whisker
d <- monthly(x) %>% mutate(Month = month(Date))
p1 <- ggplot(data = d, aes(x = factor(Month), y = Q)) +
  geom_boxplot()

p2 <- ggplot(data = d, aes(x = factor(Month), y = P)) +
  geom_boxplot()

p1 + p2 + plot_layout(ncol = 1, nrow = 2)

## Save data to file
write_csv(x, file = "test.csv")
