#' Create catchment object
#'
#' Create iMHEA catchment object.
#'
#' @param x stream_gauge
#' @param ... rain_gauge objects.
#' @param id character.
#' @param ar units
#'
#' @return A catchment object
#'
#' @examples
#' \dontrun{
#' sum(1:10)
#' }
catchment <- function(x, ..., id = NA, ar = NA) {
  UseMethod("catchment")
}

#' @export
catchment.stream_gauge <- function(x, ..., id = NA, ar = NA) {
  stopifnot(!is.na(id))
  stopifnot(!is.na(ar))
  stopifnot(inherits(ar, "units"))
  ## TODO include additional metadata
  ## gauges <- list(...)
  p_merged <- infill_precip(..., new_id = id)
  ## q <- aggregate(q, timescale = timescale)
  p_merged <- p_merged %>%
    mutate(ID_new = id, .before = ID) %>%
    update_tsibble(key = ID_new) %>%
    dplyr::select(-ID) %>%
    rename(ID = ID_new)
  q <- x %>%
    mutate(ID_new = id, .before = ID) %>%
    update_tsibble(key = ID_new) %>%
    dplyr::select(-ID) %>%
    rename(ID = ID_new)
  x <- q %>% full_join(p_merged, by = c("ID", "Date"))
  class(x) <- c("catchment", class(x))
  ## attr(x, "area") <- set_units(area, km^2)
  area(x) <- set_units(ar, km^2)
  x <- x %>% update_indices()
  x
}

#' @export
catchment.tbl_ts <- function(x, ..., id = NA, ar = NA) {
  ## TODO check validity?
  class(x) <- c("catchment", class(x))
  area(x) <- set_units(ar, km^2)
  x <- x %>% update_indices()
  x
}

#' @export
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
        fill_gaps(a$Date, a$Event, b$Date, b$Event) %>%
        setNames(c("Date", paste0("P1_", i), paste0("P2_", i)))
    }
    myfun <- function(x, y, ...) full_join(x, y, by = "Date")
    Precp_Fill_Compiled <-
      Reduce(myfun, PrecHResFill) %>%
      gather(-Date, key = "key", value = "value") %>%
      group_by(Date) %>%
      summarize(P_HRes = mean(value, na.rm = TRUE))
  } else {
    Precp_Fill_Compiled <- PrecHRes[[1]]
  }
  DateP_HRes <- Precp_Fill_Compiled$Date
  P_HRes <- Precp_Fill_Compiled$P_HRes
  out <- tibble(Date = DateP_HRes, Event = P_HRes)
  out <- tipping_bucket_rain_gauge(out, id = new_id, date_column = "Date", event_column = "Event", event_units = "mm")
}

#' Aggregate rainfall data
#'
#' Aggregate rainfall data with cubic spline interpolation.
#'
#' @param Date1 rain_gauge.
#' @param P1 rain_gauge.
#' @param Date2 rain_gauge.
#' @param P2 rain_gauge.
#' @param cutend logical.
#' @param ... Additional arguments.
#'
#' @return tsibble
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

