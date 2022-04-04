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

  Daycheck = 5 # Continuous days for recession [TODO - should be argument]

  ## Average data at daily basis
  daily_data = average(Date, Q, 1440)
  DDate = daily_data$Date
  DQ1 = daily_data$Q
  ## Consider only periods when data exist
  DQ = DQ1
  DQ[is.na(DQ)] = Inf # Is this wise?

  ## Divide the time series into non-overlapping blocks of n days
  DI = min(DDate) # Initial date
  DF = max(DDate) # Final date
  nDate = seq(DI, DF, by = paste0(Daycheck, " day"))
  k = length(nDate)
  nQmin = rep(NA, length(nDate))

  ## Calculate the minima for each block
  for (i in 2:k) {
    nQmin[i] = min(DQ[DDate >= nDate[i-1] & DData < nDate[i]])
  }
  nQmin = nQmin[2:length(nQmin)]
  nDate = nDate[2:length(nDate)]

  ## Determine the turning points of the baseflow line based on the minima
  nBQ = nQmin
  for (i in 2:(k-2)) {
    if ((0.9 * nQmin[i] > nQmin[i-1]) | (0.9 * nQmin[i] > nQmin[i+1])) {
      nBQ[i] = NA
    }
  }
  nDate = nDate[!is.na(nBQ)]
  nBQ = nBQ[!is.na(nBQ)]
  nBQ = head(nBQ, -1) # Remove last element
  nDate = head(nDate, -1) # Remove last element
  ## nBQ(end) = [];
  ## nDate(end) = [];

  ## TESTING [using MATLAB example https://uk.mathworks.com/help/matlab/ref/interp1.html]
  ## x = c(1, 2, 3, 4, 5)
  ## v = c(12, 16, 31, 10, 6)
  ## xq = c(0, 0.5, 1.5, 5.5, 6)
  ## library(Hmisc)
  ## approxExtrap(x, v, xq, rule=2)
  BQ = Hmisc::approxExtrap(nDate, nBQ, DDate, rule = 2)
  BQ[BQ > DQ] = DQ[BQ > DQ]
  BQ[is.na(DQ1)] = NA
  SQ = DQ1 - BQ

  ## Calculate the recession constant
  lim = 0.8 # Minimum R2 for linear fit
  n = length(DDate)
  R = rep(0, n)
  M = rep(0, n)
  LogBQ = log(BQ) # TODO check base is the same

  ## h = waitbar(0,'Calculating recession constant...');
  for (i in 1:n) {
    Today = DDate[i]
    X = DDate[DDate >= Today & DDate < (Today + Daycheck)]
    Y = LogBQ[DDate >= Today & DDate < (Today + Daycheck)]
    ## TODO - translate "[R(i),M(i)] = regression(X',Y');"
    stop("Haven't implemented regression method")
  }
  R = R ** 2
  M = (DDate[2] - DDate[1]) * M
  DateTau = DDate[R >= lim & M < 0]
  RTau = R[R >= lim & M < 0]
  MTau = M[R >= lim & M < 0]
  K = exp(MTau)
  k = max(K)

  ## Calculate the baseflow index
  Vb = sum(BQ[DDate >= nDate[1] & DDate <= rev(nDate)[1]])
  Va = sum(DQ1[DDate >= nDate[1] & DDate <= rev(nDate)[1]])
  BFI = Vb / Va
  ## Rescale the data
  ## Date = datetime(Date,'ConvertFrom','datenum');
  ## DDate = datetime(DDate,'ConvertFrom','datenum');
  ## %% PLOT THE RESULTS
  ## if nargin >= 4
  ##     LogQ = log(Q);
  ##     DateTau = datetime(DateTau,'ConvertFrom','datenum');
  ##     figure
  ##     subplot(3,1,1)
  ##     hold on
  ##     plot(Date,Q,DDate,DQ,DDate,BQ,DDate,SQ)
  ##     xlabel('Date')
  ##     ylabel('Discharge [l/s]')
  ##     legend('Discharge','Daily Discharge','Baseflow','Stormflow','Location','NorthWest')
  ##     box on
  ##     subplot(3,1,2)
  ##     hold on
  ##     plot(Date,LogQ,DDate,log(DQ),DDate,LogBQ)
  ##     xlabel('Date')
  ##     ylabel('Log(Discharge) log[l/s]')
  ##     legend('Discharge','Daily Discharge','Baseflow','Location','NorthWest')
  ##     box on
  ##     subplot(3,1,3)
  ##     plot(DDate(1:n),R,DDate(1:n),M,Date,LogQ,...
  ##         DateTau,LogBQ(and(R>=lim,M<0)),'o',...
  ##         DateTau,RTau,DateTau,MTau);
  ##     legend('Coeff. R^2','Regression slope','Log Discharge',...
  ##         'Identified linear','Behavioural R^2','Behavioural Slope',...
  ##         'Location','NorthWest')
  ##     box on
  ## end

}

baseflow <- function(Date, Q) {
  ## %iMHEA Baseflow separation following Chapman (1999).
  ## % [BQ,SQ,BFI,k] = iMHEA_BaseFlow(Date,Q,flag).
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % Q    = Daily Discharge [l/s].
  ## %        A regular interval in the Q time series is needed.
  ## % flag = leave empty NOT to graph plots.
  ## %
  ## % Output:
  ## % BQ  = Baseflow [l/s].
  ## % SQ  = Stormflow [l/s].
  ## % BFI = Baseflow Index [-].
  ## % k   = Recession constant [-].
  ## %
  ## % Boris Ochoa Tocachi
  ## % Imperial College London
  ## % Created in June, 2014
  ## % Modified in November, 2017

  n = length(Date)
  Daycheck = 7
  lim = 0.8 # Minimum R2 for linear fit

  R = rep(0, n)
  M = rep(0, n)
  LogQ = log(Q)

  ## h = waitbar(0,'Calculating recession constant...');
  for (i in 1:n) {
    Today = DDate[i]
    X = Date[Date >= Today & Date < (Today + Daycheck)]
    Y = LogQ[Date >= Today & Date < (Today + Daycheck)]
    ## TODO - translate "[R(i),M(i)] = regression(X',Y');"
    stop("Haven't implemented regression method")
  }
  R = R ** 2
  M = (DDate[2] - DDate[1]) * M
  DateTau = Date[R >= lim & M < 0]
  RTau = R[R >= lim & M < 0]
  MTau = M[R >= lim & M < 0]
  K = exp(MTau)
  k = max(K)

  ## % T = -1/log(K);
  ## % mT = min(T);
  ## % MT = max(T);
  ## % k = 0.949 to 0.993;
  ## % C = 1-k, or 0.018 to 0.085;
  ## % alpha = -0.01 to -0.81;
  ## % C = 1-k, or 0.011 to 0.197; with alpha
  ## C = datenum(Date(2)-Date(1))*.085;
  ## alpha = -0.1;

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
}
