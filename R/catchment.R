#' Create catchment object
#'
#' # Useful catchment methods:
#' * [`daily()`], [`monthly()`] to get daily and monthly summaries
#' * [`idc()`] to get the intensity duration curve
#' * [`fdc()`] to get the flow duration curve
#' * [`baseflow_data()`] to get baseflow data
#' * [`summary_data()`] to get all summary data
#'
#' @param x stream_gauge
#' @param ... rain_gauge objects.
#' @param id character. Catchment ID.
#' @param area units. Catchment area.
#'
#' @return A catchment object
#'
#' @examples
#'
#' \dontrun{
#' iMHEA_Catchment_AREA = read_csv(
#'   system.file("extdata", "iMHEA_Data_Areas.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' q1_raw <- read_csv(
#'   system.file("extdata", "LLO/iMHEA_LLO_01_HI_01_raw.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' p1_raw = read_csv(
#'   system.file("extdata", "LLO/iMHEA_LLO_01_PO_01_raw.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' p2_raw = read_csv(
#'   system.file("extdata", "LLO/iMHEA_LLO_01_PO_02_raw.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' # Convert precipitation data to rain_gauge objects:
#' p1 <- p1_raw %>% tipping_bucket_rain_gauge(id = "LLO_01_P0_01", event_units = "mm")
#' p2 <- p2_raw %>% tipping_bucket_rain_gauge(id = "LLO_01_P0_02", event_units = "mm")
#'
#' # Convert streamflow data to stream_gauge object:
#' q1 <-
#'   q1_raw %>%
#'   stream_gauge(
#'     id = "LLO_01_HI_01",
#'     discharge_units = "l/s",
#'     level_column = "Level cm",
#'     level_units = "cm"
#'   )
#'
#' # Define the catchment ID, and retrieve catchment area:
#' catchment_id <- "LLO_01"
#' catchment_area <-
#'   iMHEA_Catchment_AREA %>%
#'   filter(Catchment %in% catchment_id) %>%
#'   `[`(, 2, drop=T)
#'
#' # Create a catchment object (this takes a few minutes):
#' x <- catchment(q1, p1, p2, id = catchment_id, area = set_units(catchment_area, km^2))
#'
#' # Have a look at some of the attributes
#' daily(x)
#' monthly(x)
#' area(x)
#' }
#'
#' @export
catchment <- function(x, ..., id = NA, area = NA) {
  UseMethod("catchment")
}

#' @export
catchment.stream_gauge <- function(x, ..., id = NA, area = NA) {
  stopifnot(!is.na(id))
  stopifnot(!is.na(area))
  stopifnot(inherits(area, "units"))
  stopifnot(all(sapply(list(...), is_rain_gauge)))

  ## Retrieve timestep from stream_gauge object
  int_HRes <- median(lubridate::int_length(lubridate::int_diff(x[[tsibble::index(x)]])))
  timescale <- units::set_units(int_HRes, "s")

  ## Ensure timestep is consistent
  q <- aggregate(x, timescale = timescale)
  q <- q %>%
    dplyr::mutate(ID_new = id, .before = ID) %>%
    tsibble::update_tsibble(key = ID_new) %>%
    dplyr::select(-ID) %>%
    dplyr::rename(ID = ID_new)

  ## Ensure gauges have the same timestep
  gauges <- list(...)
  if (length(gauges) > 0) {
    for (i in 1:length(gauges)) {
      p <- aggregation_cs(gauges[[i]], timescale = timescale)
      gauges[[i]] <- p
    }
    if (length(gauges) > 1) {
      ## if (length(gauges) > 0) { }
      p_merged <- do.call("infill_precip", c(gauges, list(new_id = id)))
    } else {
      p_merged <- gauges[[1]]
    }
    p_merged <- p_merged %>%
      dplyr::mutate(ID_new = id, .before = ID) %>%
      tsibble::update_tsibble(key = ID_new) %>%
      dplyr::select(-ID) %>%
      dplyr::rename(ID = ID_new)
    x <- q %>% dplyr::full_join(p_merged, by = c("ID", "Date"))
  } else {
    ## Otherwise catchment object does not have precipitation data
    x <- q
  }
  ## Convert units to km^2
  catchment_area <- units::set_units(area, km^2)
  ## Compute summary data (daily/monthly/annual etc.)
  summary_data <- compute_summaries(x)
  baseflow_data <- compute_baseflow(summary_data$daily)
  obj <- new_tsibble(
    x, "area" = catchment_area,
    "indices" = list(),
    "summary_data" = summary_data,
    "baseflow_data" = baseflow_data,
    class = "catchment"
  )
  obj <- update_indices(obj)
  obj
}

#' @export
catchment.tbl_ts <- function(x, area) {
  stopifnot(inherits(area, "units"))
  x <- validate_tsibble(x)
  catchment_area <- set_units(area, km^2)
  summary_data <- compute_summaries(x)
  baseflow_data <- compute_baseflow(summary_data$daily)
  obj <- new_tsibble(
    x, "area" = catchment_area,
    "indices" = list(),
    "summary_data" = summary_data,
    "baseflow_data" = baseflow_data,
    class = "catchment"
  )
  obj <- update_indices(obj)
  obj
}

## Getters/setters for catchment objects:

#' @rdname catchment
#' @export
id <- function(x) {
  UseMethod("id")
}

#' @export
id.catchment <- function(x) {
  ## id_col <- key_vars(x)[1]
  id <- key_data(x)[["ID"]] %>% as.character() %>% unique()
  ## TODO should we check that only one ID is returned?
  id
}

#' @export
id.tbl_ts <- function(x) {
  id_col <- key_vars(x)[1]
  id <- key_data(x)[[id_col]] %>% as.character() %>% unique()
  if (id_col != "ID" | length(id) != 1)
    return(NULL)
  ## TODO should we check that only one ID is returned?
  id
}

#' @rdname catchment
#' @export
`indices<-` <- function(x, value) {
  UseMethod('indices<-')
}

#' @export
`indices<-.tbl_ts` <- function(x, value) {
  attr(x, "indices") <- value
  x
}

#' @rdname catchment
#' @export
indices <- function(x) {
  UseMethod("indices")
}

#' @export
indices.catchment <- function(x) {
  attr(x, "indices")
}

#' @rdname catchment
#' @export
`area<-` <- function(x, value) {
  UseMethod('area<-')
}

#' @export
`area<-.catchment` <- function(x, value) {
  stopifnot(inherits(value, "units"))
  attr(x, "area") <- value
  x
}

#' @rdname catchment
#' @export
area <- function(x) {
  UseMethod("area")
}

#' @export
area.catchment <- function(x) {
  attr(x, "area")
}

#' @rdname catchment
#' @export
`summary_data<-` <- function(x, value) {
  UseMethod('summary_data<-')
}

#' @export
`summary_data<-.tbl_ts` <- function(x, value) {
  attr(x, "summary_data") <- value
  x
}

#' @rdname catchment
#' @export
summary_data <- function(x) {
  UseMethod("summary_data")
}

#' @export
summary_data.catchment <- function(x) {
  attr(x, "summary_data")
}

#' @rdname catchment
#' @export
daily <- function(x) {
  UseMethod("daily")
}

#' @export
daily.catchment <- function(x) {
  attr(x, "summary_data")$daily
}

#' @rdname catchment
#' @export
monthly <- function(x) {
  UseMethod("monthly")
}

#' @export
monthly.catchment <- function(x) {
  attr(x, "summary_data")$monthly
}

#' @rdname catchment
#' @export
annual <- function(x) {
  UseMethod("annual")
}

#' @export
annual.catchment <- function(x) {
  attr(x, "summary_data")$annual
}

#' @rdname catchment
#' @export
idc <- function(x) {
  UseMethod("idc")
}

#' @export
idc.catchment <- function(x) {
  attr(x, "summary_data")$IDC
}

#' @rdname catchment
#' @export
fdc <- function(x) {
  UseMethod("fdc")
}

#' @export
fdc.catchment <- function(x) {
  attr(x, "summary_data")$FDC
}

#' @rdname catchment
#' @export
`baseflow_data<-` <- function(x, value) {
  UseMethod('baseflow_data<-')
}

#' @export
`baseflow_data<-.tbl_ts` <- function(x, value) {
  attr(x, "baseflow_data") <- value
  x
}

#' @rdname catchment
#' @export
baseflow_data <- function(x) {
  UseMethod("baseflow_data")
}

#' @export
baseflow_data.catchment <- function(x) {
  attr(x, "baseflow_data")
}

build_catchment <- function(x,
                            area = NULL,
                            indices = NULL,
                            summary_data = NULL,
                            baseflow_data = NULL,
                            update_metadata = FALSE,
                            ...) {
  valid <- is_valid_tsibble(x)
  if (!valid) {
    return(x)
  }
  if (update_metadata) {
    area <- set_units(area, km^2)
    summary_data <- compute_summaries(x)
    baseflow_data <- compute_baseflow(summary_data$daily)
    indices <- list()
  }
  obj <- new_tsibble(
    x, "area" = catchment_area,
    "indices" = indices,
    "summary_data" = summary_data,
    "baseflow_data" = baseflow_data,
    class = "catchment"
  )
  if (update_metadata)
    obj <- update_indices(obj)
  obj
}

validate_area <- function(area) area # TODO

validate_indices <- function(indices) indices # TODO

validate_summary_data <- function(summary_data) summary_data # TODO

get_abort_message <- function(x, checks) {
  if (all(checks))
    return(NULL)
  if (!checks[1]) {
    msg <- c(
      "x is not a tsibble.",
      "i" = "You must supply a valid tsibble object."
    )
    return(msg)
  }
  msg <- c()
  if (!checks[2])
    idx <- tsibble::index(x)
    msg <- c(
      msg,
      "x" = paste0("tsibble::index(x) equals ", paste(idx, collapse = ", "), "."),
      "i" = "tsibble::index(x) must equal 'Date'"
    )
  key <- key_vars(x)
  if (!checks[3])
    msg <- c(
      msg,
      "x" = paste0("x has ", length(key), " key variables."),
      "i" = paste0("x must have one key variable called 'ID'")
    )
  if (!checks[4])
    msg <- c(
      msg,
      "x" = paste0("key_vars(x) equals '", key, "'."),
      "i" = paste0("x must have one key variable called 'ID'")
    )
  if (!checks[5])
    nms <- names(x)
    msg <- c(
      msg,
      "x" = paste0("x has column name(s) ", paste(nms, collapse = ", "), "."),
      "i" = "x must contain a column 'Q' corresponding to streamflow"
    )
  msg <- c("x is not a valid tsibble.", msg)
  return(msg)
}

check_valid_tsibble <- function(x) {
  checks <- rep(FALSE, 5)
  checks[1] <- is_tsibble(x)
  if (!checks[1])
    return(checks)
  idx <- tsibble::index(x)
  checks[2] <- isTRUE(idx == "Date")
  key <- tsibble::key_vars(x)
  checks[3] <- isTRUE(length(key) == 1)
  checks[4] <- isTRUE(key[1] == "ID")
  checks[5] <- isTRUE(has_streamflow(x))
  return(checks)
}

is_valid_tsibble <- function(x) {
  isTRUE(all(check_valid_tsibble(x)))
}

validate_tsibble <- function(x) {
  checks <- check_valid_tsibble(x)
  valid <- isTRUE(all(checks))
  if (valid) {
    return(x)
  } else {
    msg <- get_abort_message(x, checks)
    rlang::abort(msg)
  }
}

has_streamflow <- function(x) {
  any(stringr::str_starts(names(x), "Q"))
}

has_precipitation <- function(x) {
  any(stringr::str_starts(names(x), "P"))
}

has_stage <- function(x) {
  any(stringr::str_starts(names(x), "H"))
}

infill_precip <- function(..., new_id) {
  ## TODO make this a part of a catchment object
  ## FIXME this is not currently used?
  PrecHRes <- list(...)
  nrg <- length(PrecHRes)
  if (nrg > 1) {
    ## Fill precipitation gaps between all combinations of rain gauges
    combinations <- combn(1:nrg, 2)
    combn_index <- dim(combinations)[2]
    PrecHResFill <- list()
    for (i in 1:combn_index) {
      a <- PrecHRes[[combinations[1,i]]]
      b <- PrecHRes[[combinations[2,i]]]
      PrecHResFill[[i]] <-
        fill_gaps(a$Date, a$P, b$Date, b$P) %>%
        setNames(c("Date", paste0("P1_", i), paste0("P2_", i)))
    }
    myfun <- function(x, y, ...) full_join(x, y, by = "Date")
    Precp_Fill_Compiled <-
      Reduce(myfun, PrecHResFill) %>%
      pivot_longer(-Date, names_to = "key", values_to = "value") %>%
      ## gather(-Date, key = "key", value = "value") %>%
      group_by(Date) %>%
      summarize(P_HRes = mean(value, na.rm = TRUE))
  } else {
    Precp_Fill_Compiled <- PrecHRes[[1]]
  }
  DateP_HRes <- Precp_Fill_Compiled$Date
  P_HRes <- Precp_Fill_Compiled$P_HRes
  out <- tibble(Date = DateP_HRes, P = P_HRes)
  out <- tipping_bucket_rain_gauge(out, id = new_id, date_column = "Date", event_column = "P", event_units = "mm")
  out
}

fill_gaps <- function(Date1,
                      P1,
                      Date2,
                      P2,
                      cutend = FALSE,
                      ...) {

  ## Check if data have the same temporal resolution
  scale1 = median(diff(Date1)) %>% as.numeric(units = "mins")
  scale2 = median(diff(Date2)) %>% as.numeric(units = "mins")
  if (scale1 != scale2) {
    scale <- max(scale1, scale2)
    x1 <- aggregation(Date1, P1, scale) # TODO
    x2 <- aggregation(Date2, P2, scale) # TODO
    Date1 <- x1$Date
    P1 <- x1$Prec
    Date2 <- x2$Date
    P2 <- x2$Prec
  } else {
    scale = round(scale1)
  }

  ## Define initial and end dates and create single vector
  DI = min(Date1[1], Date2[1])
  DF = max(rev(Date1)[1], rev(Date2)[1])
  NewDate = seq(DI, DF, by = paste0(scale, " min"))
  ## Assign data when they correspond
  NewP1 = rep(NA, length(NewDate))
  NewP2 = rep(NA, length(NewDate))
  NewP1[match(Date1, NewDate)] = P1 # FIXME what if they don't match?
  NewP2[match(Date2, NewDate)] = P2 # FIXME as above
  ## Optionally, cut vectors not to fill gaps after then end
  if (cutend) {
    ## Identify the last non-NA data in both vectors
    indexnP1 = max(which(!is.na(P1)))
    indexnP2 = max(which(!is.na(P2))) # FIXME what if all NA values?
    indexndate = min(Date1[indexnP1], Date2[indexnP2])
    ## Cut vectors after the minimum of the identified dates
    NewP1 = NewP1[NewDate <= indexndate]
    NewP2 = NewP2[NewDate <= indexndate]
    NewDate = NewDate[NewDate <= indexndate]
  }

  ## TEST IF OVERLAPPING DATA EXIST
  ## Extract all the sections where NaN data exist in any of the vectors.
  auxP1 = NewP1
  auxP2 = NewP2
  auxP1 = auxP1[!(is.na(NewP1) | is.na(NewP2))]
  auxP2 = auxP2[!(is.na(NewP1) | is.na(NewP2))]
  ## Check if any of the vectors are empty
  if (length(auxP1) <= 1) {
    ## Restore data if cut before
    if (cutend) {
      NewDate = seq(DI, DF, by = "1 min")
      ## Assign data when they correspond
      NewP1 = rep(NA, length(NewDate))
      NewP2 = rep(NA, length(NewDate))
      NewP1[match(Date1, NewDate)] = P1
      NewP2[match(Date2, NewDate)] = P2
    }
    ## return(data.frame(NewDate, NewP1, NewP2))
  }
  ## Fill data gaps
  auxCumP1 <- cumsum(auxP1)
  auxCumP2 <- cumsum(auxP2)
  mod <- lm(auxCumP2 ~ auxCumP1)
  r2 <- summary(mod)$r.squared
  coef <- coefficients(mod)[2]
  ## Fill gaps only if the correlation is almost perfect
  if (r2 < 0.99) {
    message(sprintf('The correlation is not significant as to fill the data, with R2 = %6.4f.', r2))
    if (cutend) {
      NewDate = seq(DI, DF, sep = "1 min")
      ## Assign data when they correspond
      NewP1 = rep(NA, length(NewDate))
      NewP2 = rep(NA, length(NewDate))
      NewP1[match(Date1, NewDate)] = P1
      NewP2[match(Date2, NewDate)] = P2
    }
    return(tibble(Date = NewDate, Prec1 = NewP1, Prec2 = NewP2))
  }
  NewP1[is.na(NewP1)] = NewP2[is.na(NewP1)] / coef
  NewP2[is.na(NewP2)] = NewP1[is.na(NewP2)] * coef
  ## NewP1(isnan(NewP1)) = NewP2(isnan(NewP1))/M;
  ## NewP2(isnan(NewP2)) = NewP1(isnan(NewP2))*M;
  if (cutend) {
    ## % Assign data when they correspond
    NewP1[match(Date1, NewDate)] = P1
    NewP2[match(Date2, NewDate)] = P2
  }
  ## NewDate1 = data.frame(NewDate, NewP1, NewP2)
  tibble(Date = NewDate, Prec1 = NewP1, Prec2 = NewP2)
}

