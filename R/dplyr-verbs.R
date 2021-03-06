#' @export
as_catchment <- function(x, area, ...) {
  UseMethod("as_catchment")
}

#' @keywords internal
#' @export
as_catchment.tbl_ts <- function(x, area, ...) {
  build_catchment(x, update_metadata = TRUE, ...)
}

#' @export
dplyr_row_slice.catchment <- function(data, i, ..., preserve = FALSE) {
  res <- NextMethod()
  build_catchment(res, area(data), indices(data), summary_data(data), baseflow_data(data), update_metadata = FALSE)
}

#' @export
dplyr_col_modify.catchment <- function(data, cols) {
  res <- NextMethod()
  cols_used <- attr(cols, "used")
  updated_cols <- names(cols_used)[cols_used]
  update_metadata <- FALSE
  if (any(updated_cols %in% c("Q", "H", "P")))
    update_metadata <- TRUE
  build_catchment(res, area(data), indices(data), summary_data(data), baseflow_data(data), update_metadata)
}

#' @export
`names<-.catchment` <- function(x, value) {
  res <- NextMethod()
  build_catchment(res, area(x), indices(x), summary_data(x), baseflow_data(x), update_metadata = FALSE)
}

## #' importFrom dplyr group_by_drop_default
## #' export
## group_by.catchment <- function(.data, ..., .add = FALSE,
##                                .drop = group_by_drop_default(.data)) {
##   NextMethod()
## }

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
  ## check_validity
  res <- NextMethod()
  build_catchment(x, area(template), indices(template), summary_data(template), baseflow_data(template), update_metadata = TRUE)
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

## update_meta <- function(new,
##                         old,
##                         ordered = TRUE,
##                         interval = TRUE,
##                         validate = FALSE) {
##   ## if (validate) {
##   ##   retain_tsibble(new, key = key_vars(old), index = index(old))
##   ##   validate <- FALSE
##   ## }
##   new <- as_tsibble(new) # this will validate the tsibble object
##   catchment(new, ar = area(old))
## }

