#' Compute baseflow
#'
#' TODO
#'
#' @param TODO
#' @param TODO
#' @param ... Additional arguments.
#'
#' @return TODO
#'
#' @examples
#' \dontrun{
#' sum(1:10)
#' }
baseflow_uk <- function(Date, Q, ...) {
  ## %iMHEA Baseflow separation following Gustard et al (1992).
  ## % [DDate,BQ,SQ,BFI] = iMHEA_BaseFlowUK(Date,Q,flag).
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % Q    = Daily Discharge [l/s].
  ## %        Time series will be added at daily timescale.
  ## % flag1 = leave empty NOT to calculate the recession constant.
  ## % flag2 = leave empty NOT to graph plots.
  ## %
  ## % Output:
  ## % DDate = dd/mm/yyyy hh:mm:ss [date format, daily time scale].
  ## % BQ    = Baseflow [l/s].
  ## % SQ    = Stormflow [l/s].
  ## % BFI   = Baseflow index [-].
  ## %
  ## % Boris Ochoa Tocachi
  ## % Imperial College London
  ## % Created in February, 2018
  ## % Modified in February, 2018

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

  # extract minimum Qmin for each interval; these are
  # candidates to become turning points
  df_mins <- df[df$Q==df$Qmin, ] %>% na.omit()

  # if there are two minima for an interval (e.g. two
  # days with same Q), choose the earlier one
  df_mins <- df_mins[!duplicated(df_mins$int), ]

  ## determine turning points, defined as:
  #    0.9*Qt < min(Qt-1, Qt+1)
  # do this using a weighted rolling min function
  df_mins$iQmin <- rollapply(
    df_mins$Qmin,
    width=3,
    align="center",
    fill=NA,
    FUN=function(z) which.min(z*c(1,0.9,1))
  )
  df_mins <- subset(df_mins, is.finite(iQmin))  # get rid of first/last point
  TP_day <- df_mins$day[df_mins$iQmin==2]
  TP_Qmin <- df_mins$Qmin[df_mins$iQmin==2]

  if (length(TP_day>1)){

    # linearly interpolate to length Q
    bf <- rep(NaN, length(Q))
    bf[TP_day] <- TP_Qmin
    bf <- as.numeric(zoo::na.approx(bf, na.rm=F))

    # need to fill in NAs?
    if (endrule=="Q"){
      bf[1:(TP_day[1]-1)] <- Q[1:(TP_day[1]-1)]
      bf[(rev(TP_day)[1] + 1):length(Q)] <- Q[(rev(TP_day)[1] + 1):length(Q)]
    } else if (endrule=="B") {
      bf[1:(TP_day[1] - 1)] <- bf[TP_day[1]]
      bf[(rev(TP_day)[1] + 1):length(Q)] <- bf[rev(TP_day)[1]]
    } else if (endrule != "NA") {
      stop("Invalid endrule")
    }

  } else {
    bf <- rep(0, length(Q))
  }

  # find any bf>Q and set to Q
  i_tooHigh <- which(bf>Q)
  bf[i_tooHigh] <- Q[i_tooHigh]
  return(bf)
}

## ## function [BQ,SQ] = par3(Q,k,C,alpha)
## ## % Initialise variables.
## ## BQ = zeros(size(Q));
## ## SQ = zeros(size(Q));
## ## BQ(1) = Q(1);
## ## % h = waitbar(0,'Calculating baseflow...');
## ## for i = 2:length(Q)
## ##     BQ(i) = min(k/(1+C)*BQ(i-1)+(C)/(1+C)*(Q(i)+alpha*Q(i-1)),Q(i));
## ##     SQ(i) = Q(i) - BQ(i);
## ##     % waitbar(i/length(Q))
## ## end
## ## % close(h);

## baseflow <- function(Date, Q) {
##   ## % Input:
##   ## % Date = dd/mm/yyyy hh:mm:ss [date format].
##   ## % Q    = Daily Discharge [l/s].
##   ## %        A regular interval in the Q time series is needed.
##   ## % flag = leave empty NOT to graph plots.
##   ## %
##   ## % Output:
##   ## % BQ  = Baseflow [l/s].
##   ## % SQ  = Stormflow [l/s].
##   ## % BFI = Baseflow Index [-].
##   ## % k   = Recession constant [-].
##   Date <- indices_date_input[,1,drop=T] %>% as.POSIXct(format = "%d-%b-%Y %H:%M:%S", tz = "Etc/GMT-5")
##   Q <- indices_input[,3,drop=T] %>% as.numeric()

##   na_ix <- is.finite(Q)
##   Q <- Q[na_ix]
##   Date <- Date[na_ix]
##   ## %% INITIALISE VARIABLES
##   n = length(Date) #;
##   Daycheck = 7 #; % Continuous days for recessions.
##   lim = 0.8 #; % Minimum R2 for linear fit.

##   R <- rep(0, n)
##   M <- rep(0, n)
##   LogQ <- log(Q)

##   for (i in 1:n) {
##     today <- Date[i]
##     idx <- Date >= today & Date < (today + days(Daycheck))
##     X <- Date[idx]
##     Y <- LogQ[idx]
##     mod <- lm(Y ~ X)
##     R[i] <- summary(mod)$r.squared
##     M[i] <- coef(mod)[2]
##   }

##   idx <- R >= lim & M < 0
##   DateTau <- Date[idx]
##   RTau <- R[idx]
##   MTau <- M[idx]
##   K <- exp(MTau)
##   k <- max(K)
##   ## % T = -1/log(K);
##   ## % mT = min(T);
##   ## % MT = max(T);
##   ## % k = 0.949 to 0.993;
##   ## % C = 1-k, or 0.018 to 0.085;
##   ## % alpha = -0.01 to -0.81;
##   ## % C = 1-k, or 0.011 to 0.197; with alpha
##   ## C = datenum(Date(2)-Date(1))*.085;
##   ## alpha = -0.1;

##   ## % Separate baseflow [BQ,SQ] = par3(Q,k,C,alpha).
##   ## if isempty(k)
##   ##     BQ2 = [];
##   ##     SQ2 = [];
##   ##     BFI2 = [];
##   ##     disp('These time series do not allow the determination of base flow.')
##   ##     return
##   ## else
##   ##     [BQ1] = par3(Q,k,1-k,0);
##   ##     [BQ2,SQ2] = par3(Q,k,C,0);
##   ##     [BQ3] = par3(Q,k,C*2,alpha);
##   ##     LogBQ1 = log(BQ1);
##   ##     LogBQ2 = log(BQ2);
##   ##     LogBQ3 = log(BQ3);
##   ## end

##   ## % BFI1 = sum(BQ1)/sum(Q);
##   ## BFI2 = sum(BQ2)/sum(Q);
##   ## % BFI3 = sum(BQ3)/sum(Q);

##   ## if nargin >= 3
##   ##     figure
##   ##     subplot(3,1,1)
##   ##     hold on
##   ##     plot(Date,Q,Date,BQ1,Date,BQ2,Date,BQ3,Date,SQ2)
##   ##     xlabel('Date')
##   ##     ylabel('Discharge (l/s)')
##   ##     legend('Discharge','Baseflow Linear','Baseflow 2par','Baseflow 3 par',...
##   ##         'Stormflow 2 par','Location','NorthWest')
##   ##     box on

##   ##     subplot(3,1,2)
##   ##     hold on
##   ##     plot(Date,LogQ,Date,LogBQ1,Date,LogBQ2,Date,LogBQ3)
##   ##     xlabel('Date')
##   ##     ylabel('Log(Discharge) log(l/s)')
##   ##     legend('Discharge','Baseflow Linear','Baseflow 2par','Baseflow 3 par',...
##   ##         'Location','NorthWest')
##   ##     box on

##   ##     subplot(3,1,3)
##   ##     plot(Date(1:n),R,Date(1:n),M,Date,LogQ,...
##   ##         DateTau,LogQ(and(R>=lim,M<0)),'o',...
##   ##         DateTau,RTau,DateTau,MTau);
##   ##     legend('Coeff. R^2','Regression slope','Log Discharge',...
##   ##         'Identified linear','Behavioural R^2','Behavioural Slope',...
##   ##         'Location','NorthWest')
##   ##     box on
##   ## end
##   ## ## %iMHEA Baseflow separation following Chapman (1999).
##   ## ## % [BQ,SQ,BFI,k] = iMHEA_BaseFlow(Date,Q,flag).
##   ## ## %
##   ## ## % Input:
##   ## ## % Date = dd/mm/yyyy hh:mm:ss [date format].
##   ## ## % Q    = Daily Discharge [l/s].
##   ## ## %        A regular interval in the Q time series is needed.
##   ## ## % flag = leave empty NOT to graph plots.
##   ## ## %
##   ## ## % Output:
##   ## ## % BQ  = Baseflow [l/s].
##   ## ## % SQ  = Stormflow [l/s].
##   ## ## % BFI = Baseflow Index [-].
##   ## ## % k   = Recession constant [-].
##   ## ## %
##   ## ## % Boris Ochoa Tocachi
##   ## ## % Imperial College London
##   ## ## % Created in June, 2014
##   ## ## % Modified in November, 2017

##   ## n = length(Date)
##   ## Daycheck = 7
##   ## lim = 0.8 # Minimum R2 for linear fit

##   ## R = rep(0, n)
##   ## M = rep(0, n)
##   ## LogQ = log(Q)

##   ## ## h = waitbar(0,'Calculating recession constant...');
##   ## for (i in 1:n) {
##   ##   Today = DDate[i]
##   ##   X = Date[Date >= Today & Date < (Today + Daycheck)]
##   ##   Y = LogQ[Date >= Today & Date < (Today + Daycheck)]
##   ##   ## TODO - translate "[R(i),M(i)] = regression(X',Y');"
##   ##   stop("Haven't implemented regression method")
##   ## }
##   ## R = R ** 2
##   ## M = (DDate[2] - DDate[1]) * M
##   ## DateTau = Date[R >= lim & M < 0]
##   ## RTau = R[R >= lim & M < 0]
##   ## MTau = M[R >= lim & M < 0]
##   ## K = exp(MTau)
##   ## k = max(K)

##   ## ## % T = -1/log(K);
##   ## ## % mT = min(T);
##   ## ## % MT = max(T);
##   ## ## % k = 0.949 to 0.993;
##   ## ## % C = 1-k, or 0.018 to 0.085;
##   ## ## % alpha = -0.01 to -0.81;
##   ## ## % C = 1-k, or 0.011 to 0.197; with alpha
##   ## ## C = datenum(Date(2)-Date(1))*.085;
##   ## ## alpha = -0.1;

##   ## ## % Separate baseflow [BQ,SQ] = par3(Q,k,C,alpha).
##   ## ## if isempty(k)
##   ## ##     BQ2 = [];
##   ## ##     SQ2 = [];
##   ## ##     BFI2 = [];
##   ## ##     disp('These time series do not allow the determination of base flow.')
##   ## ##     return
##   ## ## else
##   ## ##     [BQ1] = par3(Q,k,1-k,0);
##   ## ##     [BQ2,SQ2] = par3(Q,k,C,0);
##   ## ##     [BQ3] = par3(Q,k,C*2,alpha);
##   ## ##     LogBQ1 = log(BQ1);
##   ## ##     LogBQ2 = log(BQ2);
##   ## ##     LogBQ3 = log(BQ3);
##   ## ## end

##   ## ## % BFI1 = sum(BQ1)/sum(Q);
##   ## ## BFI2 = sum(BQ2)/sum(Q);
##   ## ## % BFI3 = sum(BQ3)/sum(Q);

##   ## ## if nargin >= 3
##   ## ##     figure
##   ## ##     subplot(3,1,1)
##   ## ##     hold on
##   ## ##     plot(Date,Q,Date,BQ1,Date,BQ2,Date,BQ3,Date,SQ2)
##   ## ##     xlabel('Date')
##   ## ##     ylabel('Discharge (l/s)')
##   ## ##     legend('Discharge','Baseflow Linear','Baseflow 2par','Baseflow 3 par',...
##   ## ##         'Stormflow 2 par','Location','NorthWest')
##   ## ##     box on

##   ## ##     subplot(3,1,2)
##   ## ##     hold on
##   ## ##     plot(Date,LogQ,Date,LogBQ1,Date,LogBQ2,Date,LogBQ3)
##   ## ##     xlabel('Date')
##   ## ##     ylabel('Log(Discharge) log(l/s)')
##   ## ##     legend('Discharge','Baseflow Linear','Baseflow 2par','Baseflow 3 par',...
##   ## ##         'Location','NorthWest')
##   ## ##     box on

##   ## ##     subplot(3,1,3)
##   ## ##     plot(Date(1:n),R,Date(1:n),M,Date,LogQ,...
##   ## ##         DateTau,LogQ(and(R>=lim,M<0)),'o',...
##   ## ##         DateTau,RTau,DateTau,MTau);
##   ## ##     legend('Coeff. R^2','Regression slope','Log Discharge',...
##   ## ##         'Identified linear','Behavioural R^2','Behavioural Slope',...
##   ## ##         'Location','NorthWest')
##   ## ##     box on
##   ## ## end

##   ## ## %% AUXILIARY FUNCTIONS

##   ## ## % function [BQ,SQ] = par1(Q,k)
##   ## ## % % Initialise variables.
##   ## ## % BQ = zeros(size(Q));
##   ## ## % SQ = zeros(size(Q));
##   ## ## % BQ(1) = Q(1);
##   ## ## % for i = 2:length(Q)
##   ## ## %     BQ(i) = min(k/(2-k)*BQ(i-1)+(1-k)/(2-k)*Q(i),Q(i));
##   ## ## %     SQ(i) = Q(i) - BQ(i);
##   ## ## % end
##   ## ## %
##   ## ## % function [BQ,SQ] = par2(Q,k,C)
##   ## ## % % Initialise variables.
##   ## ## % BQ = zeros(size(Q));
##   ## ## % SQ = zeros(size(Q));
##   ## ## % BQ(1) = Q(1);
##   ## ## % for i = 2:length(Q)
##   ## ## %     BQ(i) = min(k/(1+C)*BQ(i-1)+(C)/(1+C)*Q(i),Q(i));
##   ## ## %     SQ(i) = Q(i) - BQ(i);
##   ## ## % end

##   ## ## function [BQ,SQ] = par3(Q,k,C,alpha)
##   ## ## % Initialise variables.
##   ## ## BQ = zeros(size(Q));
##   ## ## SQ = zeros(size(Q));
##   ## ## BQ(1) = Q(1);
##   ## ## % h = waitbar(0,'Calculating baseflow...');
##   ## ## for i = 2:length(Q)
##   ## ##     BQ(i) = min(k/(1+C)*BQ(i-1)+(C)/(1+C)*(Q(i)+alpha*Q(i-1)),Q(i));
##   ## ##     SQ(i) = Q(i) - BQ(i);
##   ## ##     % waitbar(i/length(Q))
##   ## ## end
##   ## ## % close(h);
## }
