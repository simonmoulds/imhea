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

#' @export
dplyr_row_slice.catchment <- function(data, i, ..., preserve = FALSE) {
  res <- NextMethod()
  update_meta(res, data)
}

#' @export
dplyr_col_modify.catchment <- function(data, cols) {
  res <- NextMethod()
  ## TODO only update if cols contains Q, H, Event
  out <- update_meta(res, data)
  out
}

#' @export
`names<-.catchment` <- function(x, value) {
  res <- NextMethod()
  new_tsibble(res, "area" = area(x), "indices" = indices(x), class = "catchment")
}

#' @importFrom dplyr group_by_drop_default
#' @export
group_by.catchment <- function(.data, ..., .add = FALSE,
                               .drop = group_by_drop_default(.data)) {
  NextMethod()
}

## #' export
## group_by.rain_gauge <- group_by.catchment

## #' export
## group_by.stream_gauge <- group_by.catchment

## #' export
## dplyr_reconstruct.catchment <- function(data, template) {
##   res <- NextMethod()
##   update_meta(res, template)
## }

#' @export
dplyr_reconstruct.catchment <- function(data, template) {
  ## template <- rename_join_tsibble(data, template)
  update_meta(data, template)
}

## #' export
## dplyr_reconstruct.grouped_ts <- function(data, template) {
##   data <- grouped_df(data, group_vars(template))
##   dplyr_reconstruct.tbl_ts(data, template)
## }

## rename_join_tsibble <- function(data, template) {
##   nm <- names(template)
##   key_idx_pos <- vec_match(c(index_var(template), key_vars(template)), nm)
##   names(template)[key_idx_pos] <- names(data)[key_idx_pos]
##   template
## }

## #' @export
## dplyr_reconstruct.grouped_ts <- function(data, template) {
##   data <- grouped_df(data, group_vars(template))
##   dplyr_reconstruct.tbl_ts(data, template)
## }

## rename_join_tsibble <- function(data, template) {
##   nm <- names(template)
##   key_idx_pos <- vec_match(c(index_var(template), key_vars(template)), nm)
##   names(template)[key_idx_pos] <- names(data)[key_idx_pos]
##   template
## }

# put new data with existing attributes (update key)
update_meta <- function(new, old, ordered = TRUE, interval = TRUE,
                        validate = FALSE) {
  ## if (validate) {
  ##   retain_tsibble(new, key = key_vars(old), index = index(old))
  ##   validate <- FALSE
  ## }
  new <- as_tsibble(new) # this will validate the tsibble object
  catchment(new, ar = area(old))
}
