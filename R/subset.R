#' @export
`$.catchment` <- function(x, i) {
  NextMethod()
}

#' @export
`$.rain_gauge` <- `$.catchment`

#' @export
`$.stream_gauge` <- `$.catchment`

#' @export
`[[.catchment` <- function(x, i, j, ..., exact = TRUE) {
  NextMethod()
}

#' @export
`[[.rain_gauge` <- `[[.catchment`

#' @export
`[[.stream_gauge` <- `[[.catchment`

#' @export
`[.catchment` <- function(x, i, j, drop = FALSE) {
  res <- NextMethod()
  new_tsibble(res, "area" = area(x), "indices" = indices(x), class = "catchment")
}

#' @export
`[.rain_gauge` <- `[.catchment`

#' @export
`[.stream_gauge` <- `[.catchment`

#' @export
`$<-.catchment` <- function(x, name, value) {
  NextMethod()
}

#' @export
`$<-.rain_gauge` <- `$<-.catchment`

#' @export
`$<-.stream_gauge` <- `$<-.catchment`
