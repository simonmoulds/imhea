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
  build_catchment(res, area(x), indices(x), update_metadata = FALSE)
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
