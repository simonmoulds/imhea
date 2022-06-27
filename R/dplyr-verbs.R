## #' @export
## as_catchment <- function(x, ...) {
##   UseMethod("as_catchment")
## }

## #' @keywords internal
## #' @export
## as_catchment.tbl_ts <- function(x, ...) {
##   ## FIXME
##   build_catchment(x, ...)
## }

## build_catchment <- function(x, ...) {
##   NULL
## }

## #' @export
## dplyr_row_slice.catchment <- function(data, i, ..., preserve = FALSE) {
##   out <- dplyr_row_slice.tbl_ts(data, i, ..., preserve = FALSE)
##   build_catchment(out, data)
## }

## #' @export
## dplyr_col_modify.catchment <- function(data, cols) {
##   out <- dplyr_col_modify.tbl_ts(data, cols)
##   build_catchment(out, data)
## }

## #' @export
## dplyr_reconstruct.catchment <- function(data, template) {
##   out <- dplyr_reconstruct.tbl_ts(data, cols)
##   build_catchment(out, data)
## }
