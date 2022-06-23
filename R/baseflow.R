#' Compute baseflow
#'
#' TODO
#'
#' @param x catchment.
#' @param ... Additional arguments.
#'
#' @return TODO
#'
#' @examples
#' \dontrun{
#' sum(1:10)
#' }
baseflow_uk <- function(x, ...) {

  x_daily <- aggregate_daily(x)
  Date <- x_daily$Date
  Q <- x_daily$Q
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
    ## # need to fill in NAs?
    ## if (endrule=="Q"){
    ##   Qb[1:(TP_day[1]-1)] <- Q[1:(TP_day[1]-1)]
    ##   Qb[(rev(TP_day)[1] + 1):length(Q)] <- Q[(rev(TP_day)[1] + 1):length(Q)]

    ## } else if (endrule=="B") {
    ##   Qb[1:(TP_day[1] - 1)] <- Qb[TP_day[1]]
    ##   Qb[(rev(TP_day)[1] + 1):length(Q)] <- Qb[rev(TP_day)[1]]

    ## } else if (endrule != "NA") {
    ##   stop("Invalid endrule")
    ## }

  } else {
    Qb <- rep(0, length(Q))
  }

  # Find any Qb>Q and set to Q
  i_tooHigh <- which(Qb>Q)
  Qb[i_tooHigh] <- Q[i_tooHigh]
  return(Qb)
}

baseflow_recession_constant <- function(Date, Q, n_day = 5, ...) {
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

baseflow <- function(Date, Q, ...) {
  ## FIXME - the algorithm currently does not cope with NA
  Date <- Date[!is.na(Q)]
  Q <- Q[!is.na(Q)]
  k <- baseflow_recession_constant(Date, Q, n_day = 7)
  C <- 0.085
  alpha <- -0.1
  BQ1 <- par3(Q, k, 1-k, alpha)
  BQ2 <- par3(Q, k, C, alpha)
  SQ2 <- Q - BQ2
  BFI2 <- mean(BQ2) / mean(Q)
  BFI2
}

par3 <- function(Q, k, C, alpha) {
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

## % Separate baseflow [BQ,SQ] = par3(Q,k,C,alpha).
## if isempty(k)
##     BQ2 = [];
##     SQ2 = [];
##     BFI2 = [];
##     disp('These time series do not allow the determination of base flow.')
##     return
## else
##     [BQ1] = par3(Q,k,1-k,0);
##     [BQ2,SQ2] = par3(Q,k,C,0);
##     [BQ3] = par3(Q,k,C*2,alpha);
##     LogBQ1 = log(BQ1);
##     LogBQ2 = log(BQ2);
##     LogBQ3 = log(BQ3);
## end

## % BFI1 = sum(BQ1)/sum(Q);
## BFI2 = sum(BQ2)/sum(Q);
## % BFI3 = sum(BQ3)/sum(Q);

## if nargin >= 3
##     figure
##     subplot(3,1,1)
##     hold on
##     plot(Date,Q,Date,BQ1,Date,BQ2,Date,BQ3,Date,SQ2)
##     xlabel('Date')
##     ylabel('Discharge (l/s)')
##     legend('Discharge','Baseflow Linear','Baseflow 2par','Baseflow 3 par',...
##         'Stormflow 2 par','Location','NorthWest')
##     box on

##     subplot(3,1,2)
##     hold on
##     plot(Date,LogQ,Date,LogBQ1,Date,LogBQ2,Date,LogBQ3)
##     xlabel('Date')
##     ylabel('Log(Discharge) log(l/s)')
##     legend('Discharge','Baseflow Linear','Baseflow 2par','Baseflow 3 par',...
##         'Location','NorthWest')
##     box on

##     subplot(3,1,3)
##     plot(Date(1:n),R,Date(1:n),M,Date,LogQ,...
##         DateTau,LogQ(and(R>=lim,M<0)),'o',...
##         DateTau,RTau,DateTau,MTau);
##     legend('Coeff. R^2','Regression slope','Log Discharge',...
##         'Identified linear','Behavioural R^2','Behavioural Slope',...
##         'Location','NorthWest')
##     box on
## end

## %% AUXILIARY FUNCTIONS

## % function [BQ,SQ] = par1(Q,k)
## % % Initialise variables.
## % BQ = zeros(size(Q));
## % SQ = zeros(size(Q));
## % BQ(1) = Q(1);
## % for i = 2:length(Q)
## %     BQ(i) = min(k/(2-k)*BQ(i-1)+(1-k)/(2-k)*Q(i),Q(i));
## %     SQ(i) = Q(i) - BQ(i);
## % end
## %
## % function [BQ,SQ] = par2(Q,k,C)
## % % Initialise variables.
## % BQ = zeros(size(Q));
## % SQ = zeros(size(Q));
## % BQ(1) = Q(1);
## % for i = 2:length(Q)
## %     BQ(i) = min(k/(1+C)*BQ(i-1)+(C)/(1+C)*Q(i),Q(i));
## %     SQ(i) = Q(i) - BQ(i);
## % end

## function [BQ,SQ] = par3(Q,k,C,alpha)
## % Initialise variables.
## BQ = zeros(size(Q));
## SQ = zeros(size(Q));
## BQ(1) = Q(1);
## % h = waitbar(0,'Calculating baseflow...');
## for i = 2:length(Q)
##     BQ(i) = min(k/(1+C)*BQ(i-1)+(C)/(1+C)*(Q(i)+alpha*Q(i-1)),Q(i));
##     SQ(i) = Q(i) - BQ(i);
##     % waitbar(i/length(Q))
## end
## % close(h);

