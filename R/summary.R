#' @export
compute_summaries <- function(x, ...) {
  x_daily <- aggregate_daily(x)
  x_monthly <- aggregate_monthly(x)
  x_monthly_clim <-
    x_monthly %>%
    as_tibble() %>%
    mutate(Month = month(Date)) %>%
    group_by(Month) %>%
    summarize(
      across(any_of(c("Event")), sum, na.rm = TRUE),
      across(any_of(c("Q", "H")), mean, na.rm = TRUE)
    )
  x_annual <- aggregate_annual(x)
  x_annual_clim <-
    x_annual %>%
    as_tibble() %>%
    group_by(ID) %>%
    summarize(
      across(any_of(c("Event")), sum, na.rm = TRUE),
      across(any_of(c("Q", "H")), mean, na.rm = TRUE)
    )
  idc = compute_idc(x)
  fdc <- compute_fdc(x)
  summary_data <- list(daily = x_daily,
                       monthly = x_monthly,
                       annual = x_annual,
                       monthly_climatology = x_monthly_clim,
                       annual_climatology = x_annual_clim,
                       IDC = idc,
                       FDC = fdc)
  summary_data
}

#' @export
`summary_data<-` <- function(x, value) {
  UseMethod('summary_data<-')
}

#' @export
`summary_data<-.tbl_ts` <- function(x, value) {
  attr(x, "summary_data") <- value
  x
}

#' @export
summary_data <- function(x) {
  UseMethod("summary_data")
}

#' @export
summary_data.catchment <- function(x) {
  attr(x, "summary_data")
}
