#' @export
identify_voids <- function(x, ...) {
  Voids <- x %>% contiguous_na()
  NoVoids <- x %>% mutate(P = ifelse(is.na(P), 1, NA)) %>% contiguous_na()
  v <- nrow(Voids)
  nv <- nrow(NoVoids)
  Voids
}

contiguous_na <- function(x, ...) {
  voids <-
    x %>%
    dplyr::select(Date, P) %>%
    mutate(P = ifelse(is.na(P), 0, 1)) %>%
    mutate(group = cumsum(c(0, diff(P) != 0))) %>%
    mutate(group = c(0, diff(group))) %>%
    filter(group > 0) %>%
    dplyr::select(-group)
  voids_start <- voids$Date[voids$P == 0]
  voids_end <- voids$Date[voids$P == 1]
  if (is.na(x$P[1]))
    voids_start <- c(x$Date[1], voids_start)
  if (is.na(rev(x$P)[1]))
    voids_end <- c(voids_end, rev(x$Date)[1])
  voids <- data.frame(start = voids_start, end = voids_end)
  voids
}
