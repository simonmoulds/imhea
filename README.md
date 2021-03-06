# imhea
R package for handling iMHEA hydrometeorological data

## Installation 

Install imhea from github: 

``` r 
library(devtools)
devtools::install_github("https://github.com/simonmoulds/imhea")
```

<!-- Building the vignette will take quite a long time (about 5 minutes or so). If you do not require the vignette you can set `build_vignette = FALSE`.  -->

## Workflow 

Load the library:

```{r}
library(imhea)
```

Import some data files included with the package:

```{r}
iMHEA_Catchment_AREA = readr::read_csv(
  system.file("extdata", "iMHEA_Data_Areas.csv", package = "imhea"),
  show_col_types = FALSE
)

q1_raw <- readr::read_csv(
  system.file("extdata", "LLO/iMHEA_LLO_01_HI_01_raw.csv", package = "imhea"),
  show_col_types = FALSE
)

p1_raw = readr::read_csv(
  system.file("extdata", "LLO/iMHEA_LLO_01_PO_01_raw.csv", package = "imhea"),
  show_col_types = FALSE
)

p2_raw = readr::read_csv(
  system.file("extdata", "LLO/iMHEA_LLO_01_PO_02_raw.csv", package = "imhea"),
  show_col_types = FALSE
)
```

The package implements an object-oriented design using the [S3 system](https://adv-r.hadley.nz/s3.html). We define three classes: `rain_gauge`, `stream_gauge` and `catchment`. These extend the [tsibble](https://tsibble.tidyverts.org) class by including additional metadata on catchment properties. 

To begin with we convert the raw preciptation data to `rain_gauge` objects:

```{r}
p1 <- p1_raw |> rain_gauge(id = "LLO_01_P0_01", event_units = "mm")
p2 <- p2_raw |> rain_gauge(id = "LLO_01_P0_02", event_units = "mm")
is_rain_gauge(p1)
is_rain_gauge(p2)
```

Convert raw streamflow data to `stream_gauge` object:

```{r}
q1 <-
  q1_raw |>
  stream_gauge(
    id = "LLO_01_HI_01",
    discharge_units = "l/s",
    level_column = "Level cm",
    level_units = "cm"
  )
```

Now we prepare the additional metadata needed to build a `catchment` object, which groups together the hydrometeorological data for a specific catchment. We define the catchment ID and retrieve the catchment area from the reference dataset we obtained above:

```{r}
catchment_id <- "LLO_01"
catchment_area <-
  iMHEA_Catchment_AREA |>
  dplyr::filter(Catchment %in% catchment_id)
catchment_area <- catchment_area[1, 2, drop=TRUE]
```

Now we have everything we need to create a catchment object (this takes a few minutes):

```{r}
x <- catchment(q1, p1, p2, id = catchment_id, area = units::set_units(catchment_area, km^2))
```

The `catchment` object contains various attributes which describe the catchment properties. We can look at some catchment indices:

```{r}
indices(x)
```

We can use `ggplot2` to make some plots:

```{r}
library(ggplot2)
library(units)
library(tsibble)

p1 <- ggplot(data = daily(x), aes(x = Date, y = P)) +
  geom_line()

p2 <- ggplot(data = daily(x), aes(x = Date, y = Q)) +
  geom_line()
```

Use [patchwork](https://patchwork.data-imaginist.com)  to join plots together:

```{r}
library(patchwork)
p1 + p2 + plot_layout(ncol = 1, nrow = 2)
```

Plot monthly/annual summaries:

```{r}
p1 <- ggplot(data = monthly(x), aes(x = Date, y = Q)) +
  geom_bar(stat = "identity")

p2 <- ggplot(data = annual(x), aes(x = Date, y = Q)) +
  geom_bar(stat = "identity")

p1 + p2 + plot_layout(ncol = 1, nrow = 2)
```

Plot the intensity duration and flow duration curves: 

```{r}
ggplot(data = idc(x), aes(x = as.numeric(D), y = Intensity)) +
  geom_line() +
  scale_x_continuous(trans = "log10") +
  xlab("Duration [min]")
```

```{r}
ggplot(data = fdc(x), aes(x = Exceedance_Pct, y = as.numeric(Q))) +
  geom_line() +
  scale_y_continuous(trans = "log10") +
  ylab(expression(paste("Discharge [", m^3/s, "]")))
```

Plot baseflow:

```{r}
d <- baseflow_data(x)[["UKIH"]]
d <- d %>% tidyr::pivot_longer(cols = starts_with("Q"), names_to = "name", values_to = "values")
ggplot(data = d, aes(x = Date, y = values, col = name)) +
  geom_line()
```

Plot the monthly climatology:

```{r}
d <- monthly(x) %>% dplyr::mutate(Month = lubridate::month(Date))
p1 <- ggplot(data = d, aes(x = factor(Month), y = Q)) +
  geom_boxplot()

p2 <- ggplot(data = d, aes(x = factor(Month), y = P)) +
  geom_boxplot()

p1 + p2 + plot_layout(ncol = 1, nrow = 2)
```

## Known issues 

This package is a work in progress. There are some parts of the package that need further work:

* The package does not currently compute all the indices from the MATLAB scripts
* The package still needs a nice `print` generic function to display the various indices
* Some of the functions are rather slow, especially the internal function which interpolates precipitation data to regular intervals using cubic splines

I will be working to resolve these issues in the coming weeks. Other parts of the package may need changing depending on user needs. For example, I do not currently provide any specific plot functions because the `ggplot2` methods are relatively easy to implement given the right data. I therefore focused on developing the `catchment` class so that it easy to retrieve various data summaries needed for plotting. However, I could develop functions to produce specific plots that are needed repeatedly.

It would also be helpful to know whether there are any aspects of the MATLAB scripts which are not used, so that the R package can be as lean as possible. 

## Contact 
Simon Moulds (simon.moulds@ouce.ox.ac.uk).
