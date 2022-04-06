#' Compute indices
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

indices <- function(Date, P, Q, A, ...) {
  ## %iMHEA Hydrological indices for rainfall-runoff monitoring.
  ## % [Indices] = iMHEA_Indices(Date,P,Q,A,flag) calculates iMHEA hydrological
  ## % indices using rainfall and runoff data.
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % P = Precipitation [mm].
  ## % Q = Discharge [l/s].
  ## % A = Catchment area [km2].
  ## % flag = leave empty NOT to graph plots.
  ## %
  ## % Output:
  ## % IndicesP = Vector with iMHEA's Hydrological Indices for Precipitation.
  ## %           PYear = Annual precipitation [mm].
  ## %           DayP0 = Number of Days with zero precipitation per year [-].
  ## %           PMDry = Precipitation of driest month [mm].
  ## %           Sindx = Seasonality index [-].
  ## %           iM15m = Maximum precipitation intensity (15 min scale) [mm/h].
  ## %           iM1hr = Maximum precipitation intensity (1 hour scale) [mm/h].
  ## % PM   = Monthly precipitation (mm) per month number [Jan=1, Dec=12].
  ## % IDC  = Maximum Intensity - Duration Curve [mm/h v time].
  ## %
  ## % IndicesQ = Vector with iMHEA's Hydrological Indices for Discharge.
  ## %           Low flows:
  ## %               QDMin = Minimum daily flow [l/s/km2].
  ## %               Q95   = 95 Percentile flow from IDC [l/s/km2].
  ## %               DayQ0 = Days with zero flow per year [-].
  ## %               PQ0   = Proportion of days with zero flow per year [-].
  ## %               QMDry = Mean daily flow of driest month [l/s/km2].
  ## %           High flows:
  ## %               QDMax = Maximum Daily flow [l/s/km2].
  ## %               Q10   = 10 Percentile flow from IDC [l/s/km2].
  ## %           Mean flows:
  ## %               QDMY  = Annual Mean Daily flow [l/s/km2].
  ## %               QDML  = Long-term Mean Daily flow [l/s/km2].
  ## %               Q50   = 50 percentile flow from IDC [l/s/km2].
  ## %           Regulation:
  ## %               BFI1   = Baseflow index from UK handbook [-].
  ## %               k1     = Recession constant from UK handbook [-].
  ## %               BFI2   = Baseflow index 2-parameter algorithm [-].
  ## %               k2     = Recession constant 2-parameter algorithm [-].
  ## %               Range  = Discharge range [-] Qmax/Qmin.
  ## %               R2FDC  = Slope of the FDC between 33% and 66% / Mean flow.
  ## %               IRH   = Hydrological Regulation Index [-].
  ## %               RBI1  = Richards-Baker annual flashiness index [-].
  ## %               RBI2  = Richards-Baker seasonal flashiness index [-].
  ## %               DRYQMEAN = Min monthly flow / Mean monthly flow [-].
  ## %               DRYQWET  = Min monthly flow / Max monthly flow [-].
  ## %               SINDQ = Seasonality Index in flows [-].
  ## % QM = Monthly Mean Daily flow (l/s) per month number [Jan=1, Dec=12].
  ## % FDC  = Flow Duration Curve [l/s v %].
  ## %
  ## % RR   = Runoff Ratio [-) (Annual Discharge)/(Annual precipitation).
  ## %        a: from interannual averages
  ## %        m: from monthly averages
  ## %        l: from long-term data averages
  ## % QYEAR= Annual Discharge [mm].
  ## % Diff = Annual Precipitation - Annual Discharge [mm].
  ## % CumP = Cumulative Rainfall [mm].
  ## % CumQ = Cumulative Discharge [mm].
  ## % DP   = Daily Precipitation [mm].
  ## % DQ   = Daily Discharge and baseflow separation [l/s/km2].
  process_p(Date, P)
  process_q(Date, Q, A) # TODO

  ## Runoff coefficient
  QYEAR = IndicesQ[8] * 365 / 1000000 * 86400
  RRa = QYEAR / IndicesP[1]
  CumQ[,2] = CumQ[,2] / 1000000 * 86400
  if (is.na(QYEAR)) {
    QYEAR = mean(DQ[,2]) / 1000000 * 86400
  }
  RRl = mean(DQ[,2]) / 1000000 * 86400 / (mean(DP[,2]))

  ## Monthly discharge in mm
  MDays = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  QM = QM * MDays / 1000000 * 86400
  RRm = sum(QM) / sum(PM)
  ## TODO plots
}

## function [Climate,Indices] = iMHEA_IndicesTotal(Date,P,Q,A,varargin)
## %iMHEA All Hydrological indices for rainfall-runoff monitoring.
## % [Indices] = iMHEA_Indices(Date,P,Q,A, flag) calculates all coded
## % hydrological indices using rainfall and runoff data.
## %
## % Input:
## % Date = dd/mm/yyyy hh:mm:ss [date format].
## % P = Precipitation [mm].
## % Q = Discharge [l/s].
## % A = Catchment area [km2].
## % flag = leave empty NOT to graph plots.
## %
## % Output:
## % Climate = Vector with Precipitation Indices from 2 sets:
## %           IndicesP, ClimateP.
## % Indices = Vector with all Hydrological Indices from 3 sets:
## %           IndicesQ, IndicesPQ, IndicesPlus.
## %
## % Boris Ochoa Tocachi
## % Imperial College London
## % Created in June, 2014
## % Last edited in February, 2018

## %% PROCESS
## fprintf('\n')
## fprintf('CALCULATION OF HYDROLOGICAL AND CLIMATE INDICES OF CATCHMENT %s.\n',inputname(4))
## if nargin >= 5
##     % Calculate indices for Discharge and Precipitation.
##     [IndicesP,~,~,IndicesQ,~,~,QYEAR,RRa,RRm,RRl] = iMHEA_Indices(Date,P,Q,A,1);
##     % Calculate indices from Olden & Poff (2003).
##     [M,F,D,T,R] = iMHEA_IndicesPlus(Date,Q,A,1);
##     % Calculate Precipitation climatic indices.
##     [ClimateP] = iMHEA_ClimateP(Date,P,1);
## else
##     % Calculate indices for Discharge and Precipitation.
##     [IndicesP,~,~,IndicesQ,~,~,QYEAR,RRa,RRm,RRl] = iMHEA_Indices(Date,P,Q,A);
##     % Calculate indices from Olden & Poff (2003).
##     [M,F,D,T,R] = iMHEA_IndicesPlus(Date,Q,A);
##     % Calculate Precipitation climatic indices.
##     [ClimateP] = iMHEA_ClimateP(Date,P);
## end

## %% COMPILE
## [Climate] = [IndicesP(1:end-2);...
##              ClimateP(end);...
##              ClimateP(1:end-1);...
##              IndicesP(end-1)];
## [Indices] = [IndicesQ;...
##              QYEAR;...
##              RRa;...
##              RRm;...
##              RRl;...
##              M;...
##              F;...
##              D;...
##              T;...
##              R];

## %% PRINT RESULTS
## fprintf('\n')
## fprintf('CLIMATE INDICES:\n')
## fprintf('Magnitude and variability:\n')
## fprintf('Average annual precipitation: %8.4f [mm]\n',Climate(1))
## fprintf('Number of days with zero precipitation: %8.4f [day/year]\n',Climate(2))
## fprintf('Proportion of the year with zero precipitation: %8.4f [-]\n',Climate(3))
## fprintf('Precipitation on the driest month: %8.4f [mm]\n',Climate(4))
## fprintf('Seasonality Index: %8.4f [-]\n',Climate(5))
## fprintf('Coefficient of variation in daily precipitation: %8.4f [mm/mm]\n',Climate(6))
## fprintf('Rainfall intensity:\n')
## fprintf('Median annual maximum 1-day precipitation: %8.4f [mm/day]\n',Climate(7))
## fprintf('Median annual maximum 2-day precipitation: %8.4f [mm/2day]\n',Climate(8))
## fprintf('Median annual maximum 1-hour precipitation: %8.4f [mm/hr]\n',Climate(9))
## fprintf('Maximum 1-day precipitation intensity: %8.4f [mm/hr]\n',Climate(10))
## fprintf('Maximum 2-day precipitation intensity: %8.4f [mm/hr]\n',Climate(11))
## fprintf('Maximum 1-hour precipitation intensity: %8.4f [mm/hr]\n',Climate(12))
## fprintf('Maximum 15-min precipitation intensity: %8.4f [mm/hr]\n',Climate(13))
## fprintf('\n')
## fprintf('HYDROLOGICAL INDICES:\n')
## fprintf('Low flows:\n')
## fprintf('Minimum daily flow: %8.4f [l/s/km2]\n',Indices(1))
## fprintf('05th flow percentile (Q95) flow: %8.4f [l/s/km2]\n',Indices(2))
## fprintf('Number of days with zero flow: %8.4f [day/year]\n',Indices(3))
## fprintf('Proportion of the year with zero flow: %8.4f [-]\n',Indices(4))
## fprintf('Mean daily flow of driest month: %8.4f [l/s/km2]\n',Indices(5))
## fprintf('High flows:\n')
## fprintf('Maximum daily flow: %8.4f [l/s/km2]\n',Indices(6))
## fprintf('90th flow percentile (Q10) flow: %8.4f [l/s/km2]\n',Indices(7))
## fprintf('Mean flows:\n')
## fprintf('Annual mean daily flow: %8.4f [l/s/km2]\n',Indices(8))
## fprintf('Long-term mean daily flow: %8.4f [l/s/km2]\n',Indices(9))
## fprintf('50th flow percentile (Q50) flow: %8.4f [l/s/km2]\n',Indices(10))
## fprintf('Hydrological regulation:\n')
## fprintf('Baseflow Index from UK handbook (BFI): %8.4f [-]\n',Indices(11))
## fprintf('Average recession constant from UK handbook (k): %8.4f [-]\n',Indices(12))
## fprintf('Baseflow Index from two-parameter algorithm (BFI2): %8.4f [-]\n',Indices(13))
## fprintf('Average recession constant from two-parameter algorithm (k2): %8.4f [-]\n',Indices(14))
## fprintf('Discharge range (Qmax/Qmin): %8.4f [-]\n',Indices(15))
## fprintf('Slope of the FDC between 33%-66%: %8.4f [-]\n',Indices(16))
## fprintf('Hydrological Regulation Index (IRH): %8.4f [-]\n',Indices(17))
## fprintf('Richards-Baker annual flashiness index: %8.4f [-]\n',Indices(18))
## fprintf('Richards-Baker seasonal flashiness index: %8.4f [-]\n',Indices(19))
## fprintf('Water balance:\n')
## fprintf('Min monthly flow / Mean monthly flow: %8.4f [-]\n',Indices(20))
## fprintf('Min monthly flow / Max monthly flow: %8.4f [-]\n',Indices(21))
## fprintf('Seasonality Index in flows: %8.4f [-]\n',Indices(22))
## fprintf('Average annual discharge: %8.4f [mm]\n',Indices(23))
## fprintf('Runoff Ratio with annual averages (RRa): %8.4f [-]\n',Indices(24))
## fprintf('Runoff Ratio with monthly averages(RRm): %8.4f [-]\n',Indices(25))
## fprintf('Runoff Ratio with long-term data (RRl): %8.4f [-]\n',Indices(26))
## fprintf('\n')
## fprintf('Indices of Average Flow Magnitude:\n')
## fprintf('MA5 : Skewness in daily flows (Mean/Median): %8.4f [-]\n',Indices(27))
## fprintf('MA41: Mean annual runoff: %8.4f [l/s/km2]\n',Indices(28))
## fprintf('MA3 : Coefficient of variation in daily flows: %8.4f [-]\n',Indices(29))
## fprintf('MA11: Range 75th-25th percentiles (Q25-Q75)/Median: %8.4f [-]\n',Indices(30))
## fprintf('Indices of Low Flow Magnitude:\n')
## fprintf('ML17: 7-day min flow / mean annual daily flows: %8.4f [-]\n',Indices(31))
## fprintf('ML21: Coefficient of variation in 30-day minimum flows: %8.4f [-]\n',Indices(32))
## fprintf('ML18: Coefficient of variation in ML17: %8.4f [-]\n',Indices(33))
## fprintf('Indices of High Flow Magnitude:\n')
## fprintf('MH16: High flow discharge: Mean 90th percentile (Q10)/Median: %8.4f [-]\n',Indices(34))
## fprintf('MH14: Median maximum 30-day daily flow /Median: %8.4f [-]\n',Indices(35))
## fprintf('MH22: Mean high flow volume over 3 times median /Median: %8.4f [-]\n',Indices(36))
## fprintf('MH27: Mean high peak flow over 75th percentile (Q25)/Median: %8.4f [-]\n',Indices(37))
## fprintf('Indices of Low Flow Frequency:\n')
## fprintf('FL3 : Pulses below 5% mean daily flow/record length: %8.4f [year^-1]\n',Indices(38))
## fprintf('FL2 : Coefficient of variation in FL1: %8.4f [-]\n',Indices(39))
## fprintf('FL1 : Low flood pulse count below 25th percentile (Q75): %8.4f [year^-1]\n',Indices(40))
## fprintf('Indices of High Flow Frequency:\n')
## fprintf('FH3 : High flood pulse count over 3 median daily flow: %8.4f [year^-1]\n',Indices(41))
## fprintf('FH6 : High flow events over 3 median monthly flow: %8.4f [year^-1]\n',Indices(42))
## fprintf('FH7 : High flow events over 7 median monthly flow: %8.4f [year^-1]\n',Indices(43))
## fprintf('FH2 : Coefficient of variation in FH1: %8.4f [-]\n',Indices(44))
## fprintf('FH1 : High flood pulse count above 75th percentile (Q25): %8.4f [year^-1]\n',Indices(45))
## fprintf('Indices of Low Flow Duration:\n')
## fprintf('DL17: Coefficient of variation in DL16: %8.4f [-]\n',Indices(46))
## fprintf('DL16: Low flow pulse duration below 25th percentile (Q75): %8.4f [day]\n',Indices(47))
## fprintf('DL13: Mean of 30-day minima of daily discharge /Median: %8.4f [-]\n',Indices(48))
## fprintf('Indices of High Flow Duration:\n')
## fprintf('DH13: Mean of 30-day maxima of daily discharge /Median: %8.4f [-]\n',Indices(49))
## fprintf('DH16: Coefficient of variation in DH15: %8.4f [-]\n',Indices(50))
## fprintf('DH20: High flow pulse duration over median/0.75: %8.4f [day]\n',Indices(51))
## fprintf('DH15: High flow pulse duration over 75th percentile (Q25): %8.4f [day]\n',Indices(52))
## fprintf('Indices of Timing:\n')
## fprintf('TH3 : Max proportion of the year with no flood occurrence (<Q10): %8.4f [-]\n',Indices(53))
## fprintf('TL2 : Coefficient of variation if TL1: %8.4f [-]\n',Indices(54))
## fprintf('TL1 : Median Julian day of 1-day annual minimum: %8.4f [-]\n',Indices(55))
## fprintf('Indices of Flashiness:\n')
## fprintf('RA8 : Number of flow reversals between days: %8.4f [year^-1]\n',Indices(56))
## fprintf('RA5 : Ratio of days where flow is higher than the previous: %8.4f [-]\n',Indices(57))
## fprintf('RA6 : Median of difference between log of increasing flows: %8.4f [l/s/km2]\n',Indices(58))
## fprintf('RA7 : Median of difference between log of decreasing flows: %8.4f [l/s/km2]\n',Indices(59))
## fprintf('\n')
## fprintf('Process finished.\n')
## fprintf('\n')

process_p <- function(Date, P, ...) {
  ## %iMHEA Hydrological index calculation for Precipitation.
  ## % [Indices] = iMHEA_ProcessP(Date,P,flag) calculates rainfall indices.
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % P    = Precipitation [mm].
  ## % flag = leave empty NOT to graph plots.
  ## %
  ## % Output:
  ## % IndicesP = Vector with iMHEA's Hydrological Indices for Precipitation.
  ## %            PYear = Annual precipitation [mm].
  ## %            DayP0 = Number of days with zero precipitation per year [day].
  ## %            PP0 = Percentage of days with zero precipitation per year [-].
  ## %            PMDry = Precipitation of driest month [mm].
  ## %            Sindx = Seasonality Index [-].
  ## %            iM15m = Maximum precipitation intensity (15 min scale) [mm/h].
  ## %            iM1hr = Maximum precipitation intensity (1 hour scale) [mm/h].
  ## % PM   = Monthly precipitation [mm] per month number [Jan=1, Dec=12].
  ## % IDC  = Maximum Intensity - Duration Curve [mm/h v time].
  ## % CumP = Cumulative Precipitation [date v mm].
  ## % DP   = Daily precipitation only when data exist [date v mm].

  ## % Agregate data at daily basis.
  ## [DDate,DP,DCumP] = iMHEA_Aggregation(Date,P,1440);
  aggregation(Date, P, 1440) # TODO what should aggregation(...) return?
  ## CumP = [datenum(DDate),DCumP];

  ## Consider periods only when data exists
  NewDate = DDate[!is.na(DDate)]
  NewP = DP[!is.na(DP)]
  k = length(NewP)
  ## DP = [datenum(NewDate),NewP];

  ## % Number of Days with zero precipitation.
  ZeroP = NewP[NewP == 0]
  DayP0 = floor(365 * length(ZeroP) / k)
  PP0 = DayP0 / 365

  ## Annual and monthly aggregated data
  PM = monthly_rain(Date[!is.na(P)], P[!is.na(P)]) # TODO

  ## Precipitation in the driest month
  PMDry = min(PM)
  ## Annual precipitation
  PYear = sum(PM)
  if (is.na(PYear)) { # Why would it be NA?
    PYear = 365 * mean(NewP)
  }
  SI = (1 / PYear) * (sum(abs(PM - PYear / 12))) * 6 / 11

  ## Maximum intensity duration curve
  idc = idc(Date, P) # TODO

  indices = list(PYear, DayP0, PP0, PMDry, SINDX, iM15m, iM1hr)
  indices
  ## % Hydrological indices for precipitation.
  ## IndicesP = [PYear;...
  ##            DayP0;...
  ##            PP0;...
  ##            PMDry;...
  ##            SINDX;...
  ##            iM15m;...
  ##            iM1hr];
}

process_q <- function(Date, Q, A = NA, normalize = FALSE, ...) {
  ## %iMHEA Hydrological index calculation for Discharge.
  ## % [Indices] = iMHEA_ProcessQ(Date,Q,A,flags) calculates streamflow indices.
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % Q = Discharge [l/s].
  ## % A = Catchment area [km2] (Optional).
  ## % flag1 = leave empty NOT to graph discharge plots.
  ## % flag2 = leave empty NOT to graph baseflow plots.
  ## %
  ## % Output: [l/s/km2 if Area was input, or l/s otherwise].
  ## % IndicesQ = Vector with iMHEA's Hydrological Indices for Discharge.
  ## %           Low flows:
  ## %               QDMin = Minimum daily flow [l/s].
  ## %               Q95   = 05th percentile [l/s].
  ## %               DayQ0 = Days with zero flow per year [-].
  ## %               PQ0   = Proportion of days with zero flow per year [-].
  ## %               QMDry = Mean daily flow of driest month [l/s].
  ## %           High flows:
  ## %               QDMax = Maximum Daily flow [l/s].
  ## %               Q10   = 90th percentile [l/s].
  ## %           Mean flows:
  ## %               QDMY  = Annual Mean Daily flow [l/s].
  ## %               QDML  = Long-term Mean Daily flow [l/s].
  ## %               Q50   = 50th percentile [l/s].
  ## %           Regulation:
  ## %               BFI1   = Baseflow index from UK handbook [-].
  ## %               k1     = Recession constant from UK handbook [-].
  ## %               BFI2   = Baseflow index 2-parameter algorithm [-].
  ## %               k2     = Recession constant 2-parameter algorithm [-].
  ## %               Range = Discharge range [-] Qmax/Qmin.
  ## %               R2FDC = Slope of the FDC between 33%-66% / Mean flow [-].
  ## %               IRH   = Hydrological Regulation Index [-].
  ## %               RBI1  = Richards-Baker annual flashiness index [-].
  ## %               RBI2  = Richards-Baker seasonal flashiness index [-].
  ## %               DRYQMEAN = Min monthly flow / Mean monthly flow [-].
  ## %               DRYQWET  = Min monthly flow / Max monthly flow [-].
  ## %               SINDQ = Seasonality Index in flows [-].
  ## % QM = Monthly Mean Daily flow (l/s) per month number [Jan=1, Dec=12].
  ## % FDC  = Flow Duration Curve [l/s v %].
  ## % CumQ = Date and Cumulative Discharge [l/s].
  ## % DQ   = Daily Discharge only when data exist [date v l/s], including:
  ## %        BQ: Baseflow [l/s].
  ## %        SQ: Stormflow [l/s].

  ## %% PROCESS

  ## % Normalize discharge.
  if (normalize) {
    if (is.na(A)) {
      stop("`A` must be provided if `normalize` is TRUE")
    } else {
      Q = Q / A
    }
  }
  ## TODO

  ## % Average data at daily basis.
  average(Date, Q, 1440)
  RANGE = QDMax / QDMin
  ## CumQ = [datenum(DDate),DCumQ];

  ## % Consider periods only when data exists.
  NewDate = DDate[!is.na(DQ)]
  NewQ = DQ[!is.na(DQ)]
  l = length(NewQ)

  ## Number of days with zero flow
  ZeroQ = NewQ[NewQ == 0]
  DayQ0 = floor(365 * length(ZeroQ) / l)
  PQ0 = DayQ0 / 365

  ## Annual and monthly average data
  ## TODO
  monthly_flow(Date[!is.na(Q)], Q[!is.na(Q)])
  QMDry = min(QM)
  ## Annual average flow as mean monthly flow
  if (is.na(QDMY)) QDMY = mean(QM)
  ## Annual average flow as mean daily flow
  if (is.na(QDMY)) QDMY = mean(NewQ)

  ## Monthly discharge indices
  DRYQMEAN = QMDry / mean(QM)
  DRYQMET = QMDry / max(QM)

  ## Seasonality index
  SINDQ = (1 / (12 * QDMY)) * (sum(abs(QM - QDMY))) * (6/ 11)

  ## TODO Flow duration curve
  fdc(NewQ)
  ## [FDC,R2FDC,IRH,Ptile] = iMHEA_FDC(NewQ,1);
  ## % Percentiles from the FDC.
  ## Q95 = Ptile(1);
  ## Q50 = Ptile(4);
  ## Q10 = Ptile(7);
  baseflow_uk(Date, Q) # Gustard et al., 1992
  baseflow(NewDate, NewQ) # Chapman, 1999

  ## TODO Compile daily flows
  ## DQ = [datenum(DDate),DQ,BQ1,SQ1];

  ## % Richards-Baker flashiness index (RBI).
  Qi_1 = abs(diff(NewQ))
  RBI1 = sum(Qi_1) / sum(NewQ[2:length(NewQ)])
  Qi_2 = 0.5 * (Qi_1[1:(length(Qi_1) - 1)]) + Qi_1[2:length(Qi_1)]
  RBI2 = sum(Qi_2) / sum(NewQ[2:(length(NewQ) - 1)])

  ## % Hydrological indices for discharge.
  ## IndicesQ = [QDMin;...
  ##            Q95;...
  ##            DayQ0;...
  ##            PQ0;...
  ##            QMDry;...
  ##            QDMax;...
  ##            Q10;...
  ##            QDMY;...
  ##            QDML;...
  ##            Q50;...
  ##            BFI1;...
  ##            k1;...
  ##            BFI2;...
  ##            k2;...
  ##            RANGE;...
  ##            R2FDC;...
  ##            IRH;...
  ##            RBI1;...
  ##            RBI2;...
  ##            DRYQMEAN;...
  ##            DRYQWET;...
  ##            SINDQ];
}

monthly_flow <- function(Date, Q) {
  ## %iMHEA Calculation of monthly and annual Discharge averages.
  ## % [Q_Month,Q_Year,Q_Avg_Month,Q_Avg_Year,Q_Matrix] =
  ## % iMHEA_MonthlyRain(Date,Q,flag).
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % Q    = Discharge [l/s or l/s/km2].
  ## % flag = leave empty NOT to graph plots.
  ## %
  ## % Output:
  ## % Q_Month     = Time series of monthly discharge [l/s or l/s/km2].
  ## % Q_Year      = Time series of annual discharge [year and l/s or l/s/km2].
  ## % Q_Avg_Month = 12 average monthly discharge values [l/s or l/s/km2].
  ## % Q_Avg_Year  = Annual discharge value [l/s or l/s/km2].
  ## % Q_Matrix    = Matrix of discharge data (Year vs Months) [l/s or l/s/km2].
  ## % Q_Min_Year  = Time series of minimum annual precipitation [year and mm].
  ## % Q_Max_Year  = Time series of maximum annual precipitation [year and mm].
  ## %
  ## % Boris Ochoa Tocachi
  ## % Imperial College London
  ## % Created in September, 2017
  ## % Last edited in November, 2017

  Years = year(Date)
  n = max(Years) - min(Years) + 1 # Number of years
  Months = month(Date)

  Q_Year = rep(0, n)
  Q_YMin = matrix(data = 0, nrow = n, ncol = 2)
  Q_YMax = matrix(data = 0, nrow = n, ncol = 2)
  matrixQM1 = matrix(data = 0, nrow = 12, ncol = 1)
  sizeQM1 = matrix(data = 0, nrow = 12, ncol = 1)

  for (i in 1:n) {
    ## Annual mean
    Q_Year[i] = mean(Q[Years == (min(Years) + i - 1)])
    ## Position of the annual minimum
    MinPos = which.min(Q[Years == (min(Years) + i - 1)])
    Q_YMin[i, 1] = Q[Years == (min(Years) + i - 1)][MinPos]
    Q_YMin[i, 2] = lubridate::yday(Date[MinPos])
    MaxPos = which.max(Q[Years == (min(Years) + i - 1)])
    Q_YMax[i, 1] = Q[Years == (min(Years) + i - 1)][MaxPos]
    Q_YMax[i, 2] = lubridate::yday(Date[MaxPos])
    for (j in 1:12) {
      matrixQM1[j, i] = sum(Q[Years == (min(Years) + i - 1) & Months == j])
      sizeQM1[j, i] = length(Q[Years == (min(Years) + i - 1) & Months == j])
    }
  }
  ## TODO Generate output variables
  ## Q_Avg_Month = nansum(matrixQM1.*sizeQM1,2)./nansum(sizeQM1,2);
  ## Q_Avg_Year = nanmean(Q_Year);
  ## Q_Month = matrixQM1(:);
  ## Q_Matrix = matrixQM1';
  ## Q_Year = [(min(Years):max(Years))' , Q_Year];
  ## Q_YMin = [(min(Years):max(Years))' , Q_YMin];
  ## Q_YMax = [(min(Years):max(Years))' , Q_YMax];
  ## TODO plot results
}

monthly_rain <- function(Date, P) {
  ## %iMHEA Calculation of monthly and annual Precipitation averages.
  ## % [P_Month,P_Year,P_Avg_Month,P_Avg_Year,P_Matrix] =
  ## % iMHEA_MonthlyRain(Date,P,flag).
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % P    = Precipitation [mm].
  ## % flag = leave empty NOT to graph plots.
  ## %
  ## % Output:
  ## % P_Month     = Time series of monthly precipitation [mm].
  ## % P_Year      = Time series of annual precipitation [year and mm].
  ## % P_Avg_Month = 12 average monthly precipitation values [mm].
  ## % P_Avg_Year  = Annual precipitation value [mm].
  ## % P_Matrix    = Matrix of precipitation data (Year vs Months) [mm].
  ## % P_Min_Year  = Time series of minimum annual discharge [year and l/s or l/s/km2].
  ## % P_Max_Year  = Time series of maximum annual discharge [year and l/s or l/s/km2].
  ## %
  ## % Boris Ochoa Tocachi
  ## % Imperial College London
  ## % Created in September, 2017
  ## % Last edited in November, 2017
  Years = format(Date, "%Y") %>% as.numeric
  n = max(Years) - min(Years) + 1 # Number of years
  Months = format(Date, "%m") %>% as.numeric
  P_Year = rep(0, n)
  P_YMax = matrix(data = 0, nrow = n, ncol = 2)
  P_YMin = matrix(data = 0, nrow = n, ncol = 2)
  matrixPM1 = rep(0, 12)
  sizePM1 = rep(0, 12)
  for (i in 1:n) {
    ## Annual accumulation
    P_Year[i] = sum(P[Years = min(Years) + i - 1])
    ## Position of the annual minimum
    MinPos = which.min(P[Years == (min(Years) + i - 1)])
    P_YMin[i, 1] = P[Years == (min(Years) + i - 1)][MinPos]
    P_YMin[i, 2] = lubridate::yday(Date[MinPos])
    MaxPos = which.max(P[Years == (min(Years) + i - 1)])
    P_YMax[i, 1] = P[Years == (min(Years) + i - 1)][MaxPos]
    P_YMax[i, 2] = lubridate::yday(Date[MaxPos])
    for (j in 1:12) {
      matrixPM1[j, i] = sum(P[Years == (min(Years) + i - 1) & Months == j])
      sizePM1[j, i] = length(P[Years == (min(Years) + i - 1) & Months == j])
    }
  }

  ## P_Avg_Month = nansum(matrixPM1.*sizePM1,2)./nansum(sizePM1,2);
  P_Avg_Month = NA # TODO
  P_Avg_Year = mean(P_Year)

  P_Month = matrixPM1 # TODO
  P_Matrix = matrixPM1 # TODO
  ## P_Month = matrixPM1(:);
  ## P_Matrix = matrixPM1';
  ## P_Year = [(min(Years):max(Years))' , P_Year];
  ## P_YMin = [(min(Years):max(Years))' , P_YMin];
  ## P_YMax = [(min(Years):max(Years))' , P_YMax];
  ## TODO plotting
}

## %% PLOT RESULTS
## if nargin >= 3
##     figure
##     subplot(3,1,1)
##     bar((1:length(P_Month))',P_Month,'DisplayName',inputname(2));
##     hold on
##     grid on
##     box on
##     title('Monthly data')
##     legend('show')
##     ylabel('Precipitation [mm]')
##     set(gca,'XTick',(1:length(P_Month)),...
##         'XTickLabel',{'J','F','M','A','M','J','J','A','S','O','N','D'});

##     subplot(3,1,2)
##     bar((1:12)',P_Avg_Month,'DisplayName',inputname(2));
##     hold on
##     P_MatrixPlot = cat(1,nan(1,12),P_Matrix,nan(1,12));
##     boxplot(P_MatrixPlot,'PlotStyle','compact')
##     grid on
##     box on
##     title('Average monthly data')
##     legend('show')
##     ylabel('Precipitation [mm]')
##     set(gca,'Xlim',[0 13],...
##         'XTickLabel',{'J','F','M','A','M','J','J','A','S','O','N','D'},...
##         'XTick',(1:12));

##     subplot(3,1,3)
##     bar(P_Year(:,1),P_Year(:,2),'DisplayName',inputname(2));
##     hold on
##     box on
##     title('Annual data')
##     legend('show')
##     ylabel('Precipitation [mm]')

##     drawnow
## end

idc <- function(Date, P) {
  ## %iMHEA Calculation of Maximum Intensity-Duration Curve.
  ## % [IDC,iM15m,iM1hr] = iMHEA_IDC(Date,P,flag).
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % P    = Precipitation [mm].
  ## % flag = leave empty NOT to graph plots.
  ## %
  ## % Output:
  ## % IDC   = Maximum Intensity - Duration Curve [mm/h v time].
  ## % iM15m = Maximum precipitation intensity (15 min scale) [mm/h].
  ## % iM1hr = Maximum precipitation intensity (1 hour scale) [mm/h].
  ## %
  ## % Boris Ochoa Tocachi
  ## % Imperial College London
  ## % Created in July, 2015
  ## % Last edited in November, 2017

  ## % Maximum Intensity - Duration Curve.
  ## h = waitbar(0,'Calculating IDC...');
  ## % Consider periods only when data exists.
  VP = P
  ## Check if the measurements have 5 min interval
  if (round(median(diff(Date))) != 300) {
    ## Consider periods only when data exists
    VDate = Date[!is.na(VP)]
    VP = VP[!is.na(VP)]
    VDate = VDate[!VP == 0]
    VP = VP[!VP == 0]
    ## Aggregate data to 5 min interval [iMHEA standard?]
    ## [~,VP] = iMHEA_Aggregation(VDate,VP,5);
    aggregation(VDate, VP, 5) # TODO 5 mins
  } else {
    ## % Consider periods only when data exists.
    VP[is.na(VP)] = 0
  }
  k1 = length(VP)
  ## % Durations: 5, 10, 15, 30, 60 min; 2, 4, 12, 24 hours; 2 days.
  D = c(1, 2, 3, 6, 12, 24, 48, 144, 288, 576)
  u = rep(0, k1)
  IDC = matrix(data = 0, nrow = length(D), ncol = 2)
  IDC[,1] = D * 5 # TODO
  ## Maximum intensities
  for (i in 1:length(D)) {
    ## % Define initial IntP(1).
    u[1] = sum(VP[1:D[i]]) # The sum of the first i elements
    ## Sums i elements using a moving window
    for (j in 2:(k1 - D[i] + 1)) {
      u[j] = u[j-1] + VP[j + D[i] - 1] - VP[j-1]
    }
    IDC[i, 2] = max(u) * 12 / D[i]
    IDC[i, 3] = mean(u[u > 1e-12]) * 12 / D[i]
    IDC[i, 4] = median(u[u > 1e-12]) * 12 / D[i]
    ## IDC(i,4) = median(u(u>1E-12),'omitnan')*12/D(i);
  }

  ## % Intensity indices.
  iM15m = IDC[D == 3, 2]  # 5 * 3 = 15 min
  iM1hr = IDC[D == 12, 2] # 5 * 12 = 60 min = 1h
  ## TODO plots
}
