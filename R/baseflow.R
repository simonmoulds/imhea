#' Compute baseflow
#'
#' Implementation of UKIH baseflow separation algorithm
#'
#' @param x catchment.
#' @param ... Additional arguments.
#'
#' @return
#'
#' @references
#' Gustard, A.; Bullock, A.; Dixon, J. M.. 1992 Low flow estimation in
#' the United Kingdom. Wallingford, Institute of Hydrology, 88pp. (IH
#' Report No.108). http://nora.nerc.ac.uk/id/eprint/6050 (Accessed
#' June 2022)
#'
#' Chapman, T. (1999), A comparison of algorithms for stream flow
#' recession and baseflow separation. Hydrol. Process., 13: 701-714.
#' https://doi.org/10.1002/(SICI)1099-1085(19990415)13:5<701::AID-HYP774>3.0.CO;2-2
#' (Accessed June 2022)
#'
#' @examples
#' \dontrun{
#' sum(1:10)
#' }
baseflow <- function(Date, Q, method = "UKIH", ...) {
  stopifnot(method %in% c("UKIH", "Chapman1999"))
  if (method == "UKIH") {
    return(baseflow_ukih(Date, Q, ...))
  } else if (method == "Chapman1999") {
    return(baseflow_chapman(Date, Q, ...))
  }
}

baseflow_ukih <- function(Date, Q, ...) {
  ## x_daily <- aggregate_daily(x)
  ## Date <- x_daily$Date
  ## Q <- x_daily$Q
  Q[is.na(Q)] <- Inf

  ## Fixed interval of width 5
  int_width <- 5
  n_ints <- ceiling(length(Q) / int_width)
  ints <- rep(seq(1,n_ints), each=int_width)[1:length(Q)]
  df <- tibble(int = ints, day = seq(1, length(ints)), Q = Q)
  df <-
    df %>%
    group_by(int) %>%
    summarize(Qmin = min(Q), n_int = sum(is.finite(int))) %>%
    left_join(df, ., by="int") %>%
    subset(n_int == int_width)

  ## Extract minimum Qmin for each interval; these are
  ## candidates to become turning points
  df_mins <- df[df$Q==df$Qmin, ] %>% na.omit()

  ## If there are two minima for an interval (e.g. two
  ## days with same Q), choose the earlier one
  df_mins <- df_mins[!duplicated(df_mins$int), ]

  ## Determine turning points, defined as: 0.9*Qt < min(Qt-1, Qt+1)
  ## do this using a weighted rolling min function
  df_mins$iQmin <- zoo::rollapply(
    df_mins$Qmin,
    width = 3,
    align = "center",
    fill = NA,
    FUN = function(z) which.min(z * c(1, 0.9, 1))
  )
  df_mins <- subset(df_mins, is.finite(iQmin))  # get rid of first/last point
  TP_day <- df_mins$day[df_mins$iQmin == 2]
  TP_Qmin <- df_mins$Qmin[df_mins$iQmin == 2]

  if (length(TP_day>1)){
    # Linearly interpolate to length Q
    Qb <- rep(NaN, length(Q))
    Qb[TP_day] <- TP_Qmin
    Qb <- as.numeric(zoo::na.approx(Qb, na.rm = FALSE))
  } else {
    Qb <- rep(0, length(Q))
  }
  # Find any Qb>Q and set to Q
  i_tooHigh <- which(Qb>Q)
  Qb[i_tooHigh] <- Q[i_tooHigh]
  ## Qs <- Q - Qb
  ## x_daily <- x_daily %>% mutate(Qb_UKIH = Qb)
  ## x_daily
  Qb
}

baseflow_chapman <- function(Date, Q, ...) {
  ## FIXME - the algorithm currently does not cope with NA
  ## x_daily <- aggregate_daily(x)
  ## Date <- x_daily$Date
  ## Q <- x_daily$Q
  ## Q[is.na(Q)] <- Inf
  na_ix <- !is.na(Q)
  n <- length(Q)

  Date <- Date[!is.na(Q)]
  Q <- Q[!is.na(Q)]
  k <- compute_recession_constant(Date, Q, n_day = 7)
  C <- 0.085
  alpha <- -0.1

  myfun <- function(Q, k, C, alpha) {
    BQ <- rep(0, length(Q))
    SQ <- rep(0, length(Q))
    BQ[1] <- Q[1]
    for (i in 2:length(Q)) {
      BQ[i] <- min(
        k / (1 + C) * BQ[i-1] + C / (1 + C) * (Q[i] + alpha * Q[i-1]),
        Q[i]
      )
    }
    BQ
  }
  ## BQ1 <- myfun(Q, k, 1-k, 0)
  BQ2 <- myfun(Q, k, C, 0)
  ## SQ2 <- Q - BQ2
  ## BFI2 <- mean(BQ2) / mean(Q)
  BQ <- rep(NA, n)
  BQ[na_ix] <- BQ2
  BQ
}

compute_recession_constant <- function(Date, Q, n_day = 5, ...) {
  lim <- 0.8 # Minimum R2 for linear fit
  n <- length(Date)
  R <- rep(0, n)
  M <- rep(0, n)
  LogBQ <- log(Q)
  for (i in 1:n) {
    today <- Date[i]
    idx <- which((Date >= today) & (Date < (today + days(n_day))))
    Y <- LogBQ[idx]
    X <- seq_len(length(idx))
    if (all(is.na(Y)))
      next
    mod <- lm(Y ~ X)
    R[i] <- summary(mod)$r.squared
    M[i] <- coefficients(mod)[2]
  }
  RTau <- R[R >= lim & M < 0]
  MTau <- M[R >= lim & M < 0]
  K <- exp(MTau)
  k <- max(K, na.rm = TRUE)
  k
}

compute_baseflow_index <- function(Date, Q, Qb, ...) {
  na_ix <- is.na(Qb) | is.na(Q)
  Qb <- Qb[!na_ix]
  Q <- Q[!na_ix]
  mean(Qb) / mean(Q)
}
