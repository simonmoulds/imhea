#' Compute indices
#'
#' TODO
#'
#' @param x catchment.
#' @param area units.
#' @param ... Additional arguments.
#'
#' @return TODO
#'
#' @examples
#' \dontrun{
#' sum(1:10)
#' }
compute_indices <- function(x, area, ...) {
  UseMethod("compute_indices")
}

#' @export
compute_indices.tbl_ts <- function(x, area, ...) {
  indices_p <- process_p(as_tsibble(x))
  indices_q <- process_q(as_tsibble(x), area)
  indices <- c(indices_p, indices_q)
  indices
}

#' @export
`indices<-` <- function(x, value) {
  UseMethod('indices<-')
}

#' @export
`indices<-.tbl_ts` <- function(x, value) {
  attr(x, "indices") <- value
  x
}

#' @export
indices <- function(x) {
  UseMethod("indices")
}

#' @export
indices.catchment <- function(x) {
  attr(x, "indices")
}

#' @export
`area<-` <- function(x, value) {
  UseMethod('area<-')
}

#' @export
`area<-.catchment` <- function(x, value) {
  stopifnot(inherits(value, "units"))
  attr(x, "area") <- value
  x
}

#' @export
area <- function(x) {
  UseMethod("area")
}

#' @export
area.catchment <- function(x) {
  attr(x, "area")
}

process_p <- function(x, ...) {
  ## IndicesP = Vector with iMHEA's Hydrological Indices for Precipitation.
  ##            PYear = Annual precipitation [mm].
  ##            DayP0 = Number of days with zero precipitation per year [day].
  ##            PP0 = Percentage of days with zero precipitation per year [-].
  ##            PMDry = Precipitation of driest month [mm].
  ##            Sindx = Seasonality Index [-].
  ##            iM15m = Maximum precipitation intensity (15 min scale) [mm/h].
  ##            iM1hr = Maximum precipitation intensity (1 hour scale) [mm/h].
  ## PM   = Monthly precipitation [mm] per month number [Jan=1, Dec=12].
  ## IDC  = Maximum Intensity - Duration Curve [mm/h v time].
  ## CumP = Cumulative Precipitation [date v mm].
  ## DP   = Daily precipitation only when data exist [date v mm].
  if (!has_precipitation(x))
    return(list(PYear = NA,
                DayP0 = NA,
                PP0 = NA,
                PM = NA,
                PMDry = NA,
                SI = NA,
                IDC = NA,
                iM15m = NA,
                iM1hr = NA))

  ## Number of days with zero precipitation
  x_daily <- aggregate_daily(x)
  DayP <- x_daily$Event %>% as.numeric() %>% na.omit()
  k <- length(DayP)
  ZeroP <- DayP[DayP == 0]
  DayP0 <- floor(365 * length(ZeroP) / k)
  PP0 <- DayP0 / 365

  ## Monthly/annual salaries
  x_monthly <- aggregate_monthly(x)
  PM <- x_monthly %>%
    as_tibble() %>%
    mutate(Month = month(Date)) %>%
    group_by(Month) %>%
    summarize(Event = mean(Event), n = n())
  annual_summary <- PM %>%
    summarize(
      PMWet = max(Event),
      PMDry = min(Event),
      PYear = sum(Event)
    )
  PMDry <- annual_summary$PMDry
  PYear <- annual_summary$PYear
  ## if (is.na(PYear))
  ##   PYear <- 365 * mean(x$P)
  SI <- (1 / PYear) * (sum(abs(PM - PYear / 12))) * 6 / 11

  ## Maximum intensity duration curve
  idc = compute_idc(x) # TODO
  iM15m = idc[idc$D == set_units(15, minute), 2, drop = TRUE]
  iM1hr = idc[idc$D == set_units(60, minute), 2, drop = TRUE]
  ## Add to list
  ## TODO - add directly to object?
  indicesP = list(PYear = PYear,
                  DayP0 = DayP0,
                  PP0 = PP0,
                  PM = PM,
                  PMDry = PMDry,
                  SI = SI,
                  IDC = idc,
                  iM15m = iM15m,
                  iM1hr = iM1hr)
  indicesP
}

compute_idc <- function(x, ...) {
  Date <- x$Date
  P <- x$Event %>% as.numeric()
  scale <- median(diff(Date)) %>% as.numeric(units = "mins")
  ## TODO aggregate to 5 minute intervals if needed
  stopifnot(scale == 5) # FIXME
  P[is.na(P)] <- 0
  k1 <- length(P)
  ## Durations: 5, 10, 15, 30, 60 min, 2, 4, 12, 24, 48 hours
  D <- c(1, 2, 3, 6, 12, 24, 48, 144, 288, 576)
  u <- rep(0, k1)
  idc <- tibble(D = D * scale, Intensity = 0)
  for (i in 1:length(D)) {
    u <- zoo::rollsum(P, D[i], align = "left", na.pad = FALSE)
    idc[i,2] <- max(u, na.rm = TRUE) / (D[i] * scale) * 60 #* 12 / D[i]
  }
  idc <- idc %>%
    mutate(
      D = set_units(D, minute),
      Intensity = set_units(Intensity, mm/h)
    )
  idc
}

process_q <- function(x, area, normalize = FALSE, ...) {
  area <- set_units(area, m2)
  if (normalize)
    x <- x %>% mutate(Q = Q / area)

  ## TODO enforce minimum data availability
  x_daily <- aggregate_daily(x)
  x_monthly <- aggregate_monthly(x)
  x_annual <- aggregate_annual(x)

  QDMin <- x_daily$Q %>% min(na.rm = TRUE)
  QDMax <- x_daily$Q %>% max(na.rm = TRUE)
  QDML <- x_daily$Q %>% mean(na.rm = TRUE)
  RANGE <- QDMax / QDMin
  Q <- x$Q %>% as.numeric() %>% na.omit()
  ZeroQ <- sum(Q == 0, na.rm = TRUE)
  DayQ0 <- floor(365 * ZeroQ / length(Q))
  PQ0 <- DayQ0 / 365
  QM <- x_monthly %>%
    na.omit() %>%
    as_tibble() %>%
    mutate(Month = month(Date)) %>%
    group_by(Month) %>%
    summarize(Q = mean(Q), n = n())
  QM <- tibble(Month = 1:12) %>% left_join(QM, by = "Month")

  QDMY <- mean(x_annual$Q)
  QMDry <- min(QM$Q)
  DRYQMEAN = QMDry / mean(QM$Q)
  DRYQWET = QMDry / max(QM$Q)

  ## Seasonality index
  SI <- (1 / (12 * QDMY)) * (sum(abs(QM$Q - QDMY))) * 6 / 11

  ## Flow Duration Curve, FDC Slope, and IRH.
  fdc <- compute_fdc(x)
  Q95 <- fdc$Q[fdc$Exceedance_Pct %in% 95]
  Q75 <- fdc$Q[fdc$Exceedance_Pct %in% 75]
  Q66 <- fdc$Q[fdc$Exceedance_Pct %in% 66]
  Q50 <- fdc$Q[fdc$Exceedance_Pct %in% 50]
  Q33 <- fdc$Q[fdc$Exceedance_Pct %in% 33]
  Q25 <- fdc$Q[fdc$Exceedance_Pct %in% 25]
  Q10 <- fdc$Q[fdc$Exceedance_Pct %in% 10]
  R2FDC <- (log10(Q66) - log10(Q33)) / (0.66 - 0.33)

  ## Hydrological regulation index
  auxFDC <- fdc$Q
  auxFDC[fdc$Exceedance_Pct < 50] = Q50
  IRH <- sum(auxFDC) / sum(fdc$Q)

  ## Compute baseflow using daily data
  BQ1 <- baseflow(x_daily$Date, x_daily$Q, method = "UKIH")
  BFI1 <- compute_baseflow_index(x_daily$Date, x_daily$Q, BQ1)
  k1 <- compute_recession_constant(x_daily$Date, BQ1)
  BQ2 <- baseflow(x_daily$Date, x_daily$Q, method = "Chapman1999")
  BFI2 <- compute_baseflow_index(x_daily$Date, x_daily$Q, BQ2)
  k2 <- compute_recession_constant(x_daily$Date, x_daily$Q, n_day = 7)

  ## Richards-Baker flashiness index (RBI).
  ## FIXME - minimum data availability?
  DQ <- x_daily$Q %>% na.omit()
  n_day <- length(DQ)
  Qi_1 = abs(diff(DQ))
  RBI1 = sum(Qi_1, na.rm = TRUE) / sum(DQ[2:n_day], na.rm = TRUE)
  Qi_2 = 0.5 * (Qi_1[1:(n_day - 1)] + Qi_1[2:n_day])
  RBI2 = sum(Qi_2, na.rm = TRUE) / sum(DQ[2:(n_day - 1)], na.rm = TRUE)

  indicesQ <- list(QDMin = QDMin,
                   Q95 = Q95,
                   DayQ0 = DayQ0,
                   PQ0 = PQ0,
                   QMDry = QMDry,
                   QDMax = QDMax,
                   Q10 = Q10,
                   QDMY = QDMY,
                   QDML = QDML,
                   Q50 = Q50,
                   BFI1 = BFI1,
                   k1 = k1,
                   BFI2 = BFI2,
                   k2 = k2,
                   RANGE = RANGE,
                   R2FDC = R2FDC,
                   IRH = IRH,
                   RBI1 = RBI1,
                   DRYQMEAN = DRYQMEAN,
                   DRYQWET = DRYQWET,
                   SINDQ = SI)
  indicesQ
}

compute_fdc <- function(x, ...) {
  ## FIXME minimum time interval?
  Q <- x$Q %>% as.numeric() %>% na.omit()
  k <- length(Q)
  pct <- 100 * (1 - ((1:k) - .44) / (k + .12))
  Q <- sort(Q)
  pct_out <- 0:100
  Q_out <- sapply(
    pct_out,
    FUN=function(xout)
      spline(x = pct, y = Q, xout = xout)$y
  )
  tibble(Exceedance_Pct = pct_out, Q = Q_out)
}

## monthly_flow <- function(Date, Q) {
##   ## %iMHEA Calculation of monthly and annual Discharge averages.
##   ## % [Q_Month,Q_Year,Q_Avg_Month,Q_Avg_Year,Q_Matrix] =
##   ## % iMHEA_MonthlyRain(Date,Q,flag).
##   ## %
##   ## % Input:
##   ## % Date = dd/mm/yyyy hh:mm:ss [date format].
##   ## % Q    = Discharge [l/s or l/s/km2].
##   ## % flag = leave empty NOT to graph plots.
##   ## %
##   ## % Output:
##   ## % Q_Month     = Time series of monthly discharge [l/s or l/s/km2].
##   ## % Q_Year      = Time series of annual discharge [year and l/s or l/s/km2].
##   ## % Q_Avg_Month = 12 average monthly discharge values [l/s or l/s/km2].
##   ## % Q_Avg_Year  = Annual discharge value [l/s or l/s/km2].
##   ## % Q_Matrix    = Matrix of discharge data (Year vs Months) [l/s or l/s/km2].
##   ## % Q_Min_Year  = Time series of minimum annual precipitation [year and mm].
##   ## % Q_Max_Year  = Time series of maximum annual precipitation [year and mm].
##   ## %
##   ## % Boris Ochoa Tocachi
##   ## % Imperial College London
##   ## % Created in September, 2017
##   ## % Last edited in November, 2017

##   Years = year(Date)
##   n = max(Years) - min(Years) + 1 # Number of years
##   Months = month(Date)

##   Q_Year = rep(0, n)
##   Q_YMin = matrix(data = 0, nrow = n, ncol = 2)
##   Q_YMax = matrix(data = 0, nrow = n, ncol = 2)
##   matrixQM1 = matrix(data = 0, nrow = 12, ncol = 1)
##   sizeQM1 = matrix(data = 0, nrow = 12, ncol = 1)

##   for (i in 1:n) {
##     ## Annual mean
##     Q_Year[i] = mean(Q[Years == (min(Years) + i - 1)])
##     ## Position of the annual minimum
##     MinPos = which.min(Q[Years == (min(Years) + i - 1)])
##     Q_YMin[i, 1] = Q[Years == (min(Years) + i - 1)][MinPos]
##     Q_YMin[i, 2] = lubridate::yday(Date[MinPos])
##     MaxPos = which.max(Q[Years == (min(Years) + i - 1)])
##     Q_YMax[i, 1] = Q[Years == (min(Years) + i - 1)][MaxPos]
##     Q_YMax[i, 2] = lubridate::yday(Date[MaxPos])
##     for (j in 1:12) {
##       matrixQM1[j, i] = sum(Q[Years == (min(Years) + i - 1) & Months == j])
##       sizeQM1[j, i] = length(Q[Years == (min(Years) + i - 1) & Months == j])
##     }
##   }
##   ## TODO Generate output variables
##   ## Q_Avg_Month = nansum(matrixQM1.*sizeQM1,2)./nansum(sizeQM1,2);
##   ## Q_Avg_Year = nanmean(Q_Year);
##   ## Q_Month = matrixQM1(:);
##   ## Q_Matrix = matrixQM1';
##   ## Q_Year = [(min(Years):max(Years))' , Q_Year];
##   ## Q_YMin = [(min(Years):max(Years))' , Q_YMin];
##   ## Q_YMax = [(min(Years):max(Years))' , Q_YMax];
##   ## TODO plot results
## }

## monthly_rain <- function(Date, P) {
##   ## % iMHEA Calculation of monthly and annual Precipitation averages.
##   ## % Input:
##   ## % Date = dd/mm/yyyy hh:mm:ss [date format].
##   ## % P    = Precipitation [mm].
##   ## % flag = leave empty NOT to graph plots.
##   ## %
##   ## % Output:
##   ## % P_Month     = Time series of monthly precipitation [mm].
##   ## % P_Year      = Time series of annual precipitation [year and mm].
##   ## % P_Avg_Month = 12 average monthly precipitation values [mm].
##   ## % P_Avg_Year  = Annual precipitation value [mm].
##   ## % P_Matrix    = Matrix of precipitation data (Year vs Months) [mm].
##   ## % P_Min_Year  = Time series of minimum annual discharge [year and l/s or l/s/km2].
##   ## % P_Max_Year  = Time series of maximum annual discharge [year and l/s or l/s/km2].
##   Years = format(Date, "%Y") %>% as.numeric
##   n = max(Years) - min(Years) + 1 # Number of years
##   Months = format(Date, "%m") %>% as.numeric
##   P_Year = rep(0, n)
##   P_YMax = matrix(data = 0, nrow = n, ncol = 2)
##   P_YMin = matrix(data = 0, nrow = n, ncol = 2)
##   matrixPM1 = rep(0, 12)
##   sizePM1 = rep(0, 12)
##   for (i in 1:n) {
##     ## Annual accumulation
##     P_Year[i] = sum(P[Years = min(Years) + i - 1])
##     ## Position of the annual minimum
##     MinPos = which.min(P[Years == (min(Years) + i - 1)])
##     P_YMin[i, 1] = P[Years == (min(Years) + i - 1)][MinPos]
##     P_YMin[i, 2] = lubridate::yday(Date[MinPos])
##     MaxPos = which.max(P[Years == (min(Years) + i - 1)])
##     P_YMax[i, 1] = P[Years == (min(Years) + i - 1)][MaxPos]
##     P_YMax[i, 2] = lubridate::yday(Date[MaxPos])
##     for (j in 1:12) {
##       matrixPM1[j, i] = sum(P[Years == (min(Years) + i - 1) & Months == j])
##       sizePM1[j, i] = length(P[Years == (min(Years) + i - 1) & Months == j])
##     }
##   }

##   ## P_Avg_Month = nansum(matrixPM1.*sizePM1,2)./nansum(sizePM1,2);
##   P_Avg_Month = NA # TODO
##   P_Avg_Year = mean(P_Year)

##   P_Month = matrixPM1 # TODO
##   P_Matrix = matrixPM1 # TODO
##   ## TODO plotting
##   list(P_Month = P_Month,
##        P_Year = P_Year,
##        P_Avg_Month = P_Avg_Month,
##        P_Avg_Year = P_Avg_Year,
##        P_Matrix = P_Matrix,
##        P_Min_Year = P_Min_Year,
##        P_Max_Year = P_Max_Year)
## }

indices_plus <- function(Date, Q, A, normalize, ...) {
  ## % iMHEA Hydrological indices from Olden & Poff (2003).
  ## % [M,F,D,T,R] = iMHEA_IndicesPlus(Date,P,Q,flag) calculates hydrological
  ## % indices from Olden & Poff (2003) using discharge data.
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % Q = Discharge [l/s or l/s/km2].
  ## % A = Catchment area [km2] (Optional).
  ## % flag = leave empty NOT to graph plots.
  ## %
  ## % Output:
  ## % [M,F,D,T,R] = Hydrological Indices from Olden & Poff (2003).

  ## % Normalize discharge.
  ## if nargin >= 3
  ##     Q = Q/A;
  ## end

  ## Average data at daily basis.
  ## [DDate,DQ,~,~,QDML] = iMHEA_Average(Date,Q,1440);
  stopifnot(normalize & !is.na(A) | !normalize)
  if (normalize)
    Q <- Q / A

  ## Average data at daily basis.
  voids = identify_voids(tibble(Date = Date, Event = Q))
  x <- tibble(Date = Date, Q = Q) %>%
    mutate(Date = ceiling_date(Date, unit = "1 day")) %>%
    group_by(Date) %>%
    summarize(Q = sum(Q, na.rm = TRUE)) %>%
    mutate(CumQ = cumsum(Q))
  for (i in 1:nrow(voids)) {
    idx <- which(x$Date > voids[i,1] & x$Date < voids[i,2])
    x[idx,] = NA
  }
  x <- x %>% na.omit()
  NewDate <- x$Date
  NewQ <- x$Q
  QDML <- mean(NewQ)
  k <- length(NewQ)

  ## Median of daily flows
  MA2 <- median(DQ)
  if (MA2 == 0) {
    MA2 <- QDML
    warning("Median of flows is zero. Assigned mean instead.")
  }

  ## Variability in daily flows
  MA3 <- sd(NewQ) / QDML
  ## Skewness in daily flows
  MA5 <- QDML / MA2

  fdc <- flow_duration_curve(NewQ)
  Q75 <- fdc_percentile(fdc, 75)
  Q25 <- fdc_percentile(fdc, 25)
  Q10 <- fdc_percentile(fdc, 10)
  Q1 <- fdc_percentile(fdc, 1)

  ## High flow discharge
  MH15 <- Q1 / MA2
  MH16 <- Q10 / MA2
  MH17 <- Q25 / MA2

  ## Spread in daily flows
  MA11 <- (Q25 - Q75) / MA2

  ## % Annual and monthly average data.
  ## [~,Q_Year,QDMM,~,~,Q_YMin,~] = iMHEA_MonthlyFlow(NewDate,NewQ);
  ## % [~,Q_Year,QDMM,~,Q_Matrix,Q_Min_Year,~] = iMHEA_MonthlyFlow(NewDate,NewQ);
  ## % Variability in monthly flows.
  ## MonthMedian = median(QDMM);
  ## % MonthIQR = iqr(QDMM);
  ## % MA37 = MonthIQR / MonthMedian;

  ## % Mean and minimum monthly flows.
  ## YearFlow = Q_Year(:,2);
  ## MA41 = mean(YearFlow);
  ## % QDMMin = nanmin(Q_Matrix);
  ## % MA12 = QDMM(1);
  ## % MA19 = QDMM(7);
  ## % MA18 = QDMM(6);
  ## % ML4  = QDMMin(4);
  ## % ML5  = QDMMin(5);
  ## % ML9  = QDMMin(9);
  ## % ML10 = QDMMin(10);

  ## 30-day maxima and minima of daily discharge
  Daycheck <- 30
  Max30 <- rep(0, k)
  Min30 <- rep(0, k)
  for (i in 1:k) {
    Today <- NewDate[i]
    Y <- NewQ[NewDate >= Today & NewDate < (Today + Daycheck)]
    Max30[i] <- max(Y)
    Min30[i] <- min(Y)
  }
  DH13 <- mean(Max30) / MA2
  DL13 <- mean(Min30) / MA2
  ML21 <- sd(Min30) / mean(Min30)
  MH14 <- median(Max30) / MA2

  ## 7-day minima of daily discharge for baseflow index
  Daycheck <- 7
  Min7 <- rep(0, k)
  for (i in 1:k) {
    Today <- NewDate[i]
    Y <- NewQ[NewDate >= Today & NewDate < (Today + Daycheck)]
    Min7[i] <- min(Y)
  }
  ML17 <- min(Min7) / MA41
  ML18 <- sd(Min7) / mean(Min7)

  ## %% TIMING
  ## % Julian Date (day number of current year) of annual minimum.
  ## YearMinD = Q_YMin(:,3);
  ## TL1  = median(YearMinD);
  ## TL2  = std(YearMinD) / mean(YearMinD);

  ## % Over Q25 (75th percentile).
  ## [MH27,~,FH12,DH1516] = iMHEA_Pulse(NewDate,NewQ,Q25);
  ## MH27 = MH27(2) / MA2;
  ## FH1 = FH12(1)*365 / datenum(NewDate(end)-NewDate(1)+1);
  ## FH2 = FH12(5);
  ## DH15 = DH1516(2);
  ## DH16 = DH1516(5);

  ## % Over 3 times Median.
  ## [~,MH22,FH3] = iMHEA_Pulse(NewDate,NewQ,3*MA2);
  ## FH3 = FH3(1)*365 / datenum(NewDate(end)-NewDate(1));
  ## MH22 = MH22(2) / MA2;

  ## % Over 7 times Median.
  ## % [MH26,MH23,FH4] = iMHEA_Pulse(NewDate,NewQ,7*MA2);
  ## % FH4 = FH4(1)*365/datenum(NewDate(end)-NewDate(1));
  ## % MH26 = MH26(2) / MA2;
  ## % MH23 = MH23(2) / MA2;

  ## % Over 3 times Monthly Median.
  ## [~,~,FH6] = iMHEA_Pulse(NewDate,NewQ,3*MonthMedian);
  ## FH6 = FH6(1)*365 / datenum(NewDate(end)-NewDate(1)+1);

  ## % Over 7 times Monthly Median.
  ## [~,~,FH7] = iMHEA_Pulse(NewDate,NewQ,7*MonthMedian);
  ## FH7 = FH7(2)*365/datenum(NewDate(end)-NewDate(1)+1);

  ## % Below Q75 (25th percentile).
  ## [~,~,~,~,~,~,~,FL12,DL1617] = iMHEA_Pulse(NewDate,NewQ,Q75);
  ## FL1 = FL12(1)*365 / datenum(NewDate(end)-NewDate(1));
  ## FL2 = FL12(5);
  ## DL16 = DL1617(2);
  ## DL17 = DL1617(5);

  ## % Below 5% of QDML.
  ## [~,~,~,~,~,~,~,FL3] = iMHEA_Pulse(NewDate,NewQ,0.05*QDML);
  ## FL3 = FL3(1)*365 / datenum(NewDate(end)-NewDate(1)+1);

  ## % Over median/0.75.
  ## [~,~,~,DH20] = iMHEA_Pulse(NewDate,NewQ,MA2/0.75);
  ## DH20 = DH20(2);

  ## % Below Q10 (90th percentile).
  ## [~,~,~,~,TH3] = iMHEA_Pulse(NewDate,NewQ,Q10);
  ## TH3 = TH3/365;

  ## Rate of change
  LogQ <- log(NewQ)
  DiffLogQ <- diff(LogQ)
  RA6 <- median(DiffLogQ[DiffLogQ > 0])
  RA7 <- median(DiffLogQ[DiffLogQ < 0])

  ## Number of increasing and decreasing flows (reversals) between days
  RA8 <- 0
  RA5 <- 0
  for (i in 1:(k-1)) {
    if (NewQ[i+1] > NewQ[i] & reversal == -1) {
      RA8 <- RA8 + 1
      reversal <- 1
    } else if (NewQ[i+1] < NewQ[i] & reversal == 1) {
      RA8 <- RA8 + 1
      reversal <- -1
    }
    if (NewQ[i+1] > NewQ[i]) {
      RA5 <- RA5 + 1
    }
  }
  n_days <- as.numeric(max(NewDate) - min(NewDate), units = "days")
  RA8 <- RA8 / n_days
  RA5 <- RA5 / n_days

  ## M = [MA5;...
  ##     MA41;...
  ##     MA3;...
  ##     MA11;...
  ##     ML17;...
  ##     ML21;...
  ##     ML18;...
  ##     MH16;...
  ##     MH14;...
  ##     MH22;...
  ##     MH27];

  ## F = [FL3;...
  ##     FL2;...
  ##     FL1;...
  ##     FH3;...
  ##     FH6;...
  ##     FH7;...
  ##     FH2;...
  ##     FH1];

  ## D = [DL17;...
  ##     DL16;...
  ##     DL13;...
  ##     DH13;...
  ##     DH16;...
  ##     DH20;...
  ##     DH15];

  ## T = [TH3;...
  ##     TL2;...
  ##     TL1];

  ## R = [RA8;...
  ##     RA5;...
  ##     RA6;...
  ##     RA7];
}

climate_p <- function(...) {
  ## %imhea Catchment climatic characteristics derived from precipitation.
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % P = Precipitation [mm].
  ## %
  ## % Output:
  ## % ClimateP = Climatic characteristics derived from precipitation.
  ## %            RMED1D = Median annual maximum 1-day precipitation [mm].
  ## %            RMED2D = Median annual maximum 2-day precipitation [mm].
  ## %            RMED1H = Median annual maximum 1-hour precipitation [mm].
  ## %           *iMAX1D = Maximum 1-day precipitation intensity [mm/h].
  ## %           *iMAX2D = Maximum 2-day precipitation intensity [mm/h].
  ## %           *iMAX1H = Maximum 1-hour precipitation intensity [mm/h].
  ## %            PVAR   = Coefficient of variation in precipitation [-].
  x_day <- aggregate_p(P, Date, unit = "1 day")
  x_hour <- aggregate_p(P, Date, unit = "1 hour")
  DDate <- x_day$Date
  DP <- x_day$P
  HDate <- x_hour$Date
  HP <- x_hour$P

  NewDate <- x_day$Date
  NewP <- x_day$P
  NewHDate <- x_hour$Date
  NewHP <- x_hour$P

  ## Mean and variation in daily precipitation.
  PBAR <- mean(NewP)
  PSTD <- sd(NewP)
  PVAR <- PSTD / PBAR

  x_2day <- aggregate_p(NewP, NewDate, unit = "2 day")
  Sum2D <- x_2day$P

  PYMax1D <- monthly_rain(NewDate, NewP)$P_Max_Year
  PYMax2D <- monthly_rain(NewDate, Sum2D)$P_Max_Year
  PYMax1H <- monthly_rain(NewHDate, NewHP)$P_Max_Year
  RMED1D <- median(PYMax1D[,2])
  RMED2D <- median(PYMax2D[,2])
  RMED1H <- median(PYMax1H[,2])

  idc <- idc_fun(Date, P)
  ## intensity indices
  ## FIXME
  ## Durations: 5, 10, 15, 30, 60 min; 2, 4, 12, 24 hours; 2 days.
  ## D = [1 2 3 6 12 24 48 144 288 576];
  iMAX1D <- idc$Intensity[idc$D == 288]
  iMAX2D <- idc$Intensity[idc$D == 576]
  iMAX1H <- idc$Intensity[idc$D == 12]

  ## % Physical characteristics derived from precipitation.
  list(RMED1D = RMED1D,
       RMED2D = RMED2D,
       RMED1H = RMED1H,
       iMAX1D = iMAX1D,
       iMAX2D = iMAX2D,
       iMAX1H = iMAX1H,
       PVAR = PVAR)
}

pulse <- function(Date, Q, Lim, ...) {
  ## function [MH,VH,FH,DH,TH,ML,VL,FL,DL,TL] = iMHEA_Pulse(Date,Q,Lim,varargin)
  ## %iMHEA Pulse count.
  ## % [MH,VH,FH,DH,TH,ML,VL,FL,DL,TL] = iMHEA_Pulse(Date,Q,Lim,flag) calculates
  ## % magnitude, volume, frequency, duration, timing and coefficientes of
  ## % variation for each property, of pulses in Q over the threshold Lim.
  ## %
  ## % Input:
  ## % Date = dd/mm/yyyy hh:mm:ss [date format].
  ## % Q    = Discharge [l/s or l/s/km2].
  ## % Lim  = Threshold [l/s or l/s/km2].
  ## % flag = leave empty NOT to graph plots or print results.
  ## %
  ## % Output:
  ## % M = (Total Average Min Max CVar) magnitude of pulses [l/s].
  ## % V = (Total Average Min Max CVar) volume of pulses [l].
  ## % F = (Total Average Min Max CVar) frequency of pulses [/year].
  ## % D = (Total Average Min Max CVar) duration of pulses [day].
  ## % T = Max period with no pulse occurrence [day].
  ## %
  ## % These variables are defined for:
  ## %      H = High pulses.
  ## %      L = Low pulses.

  ## Initialize variables
  Years <- year(Date)
  HFreq <- tibble(Year = unique(Years) %>% sort(), Q = 0)
  LFreq <- HFreq
  Date <- Date[!is.na(Q)]
  Q <- Q[!is.na(Q)]
  k <- length(Q)
  ## Counters
  nH <- nL <- 1
  HPeriod <- LPeriod <- 0
  HPeak <- LPeak <- Inf
  HVal <- LVal <- 0
  ## Subtract threshold
  ModQ <- Q - Lim
  Date <- c(Date, rev(Date)[1])
  ModQ <- c(ModQ, rev(ModQ)[1])

  for (j in 1:k) {
    if (ModQ[j] >= 0) {
      nH <- nH + 1
      row_ix <- which(HFreq$Year %in% Years[j])
      HFreq$Q[row_ix] <- HFreq$Q[row_ix] + 1
      HPeriod[nH, 1] <- Date[j] # Initial date of high pulse interval
      HPeriod[nH, 2] <- Date[j+1] # Final date of high pulse interval
      HPeak[nH, 1] = Q[j]         # Pulse peak
      HVol[nH] = (
        as.numeric(Date[j+1] - Date[j], units = "days")
        * (max(ModQ[j+1], 0) + max(ModQ[j], 0)) / 2
      )
      if (HPeriod(nH, 1) == HPeriod[nH-1, 2]) {
        ## Aggregate continuous pulses
        nH <- nH - 1
        HFreq$Q[row_ix] <- HFreq[row_ix] - 1
        HPeriod[nH, 2] <- HPeriod[nH+1, 2]
        HPeriod[nH+1,] <- NA
        HPeak[nH, 1] <- max(HPeak[nH,1], HPeak[nH+1])
        HPeak[nH+1] <- NA
        HVol[nH] <- HVol[nH+1] + HVol[nH]
        HVol[nH+1] <- NA
      }
    } else {
      nL <- nL + 1
      LFreq[row_ix] <- LFreq[row_ix] + 1
      LPeriod[nL, 1] <- Date[j]
      LPeriod[nL, 2] <- Date[j+1]
      LPeak[nL, 1] <- Q[j]
      LVol[nH] = (
        as.numeric(Date[j+1] - Date[j], units = "days")
        * (min(ModQ[j+1], 0) + min(ModQ[j], 0)) / 2
      )
      if (LPeriod[nL, 1] == LPeriod[nL-1,2]) {
        ## Aggregate continuous pulses
        nL <- nL - 1
        LFreq[row_ix] <- LFreq[row_ix] - 1
        LPeriod[nL, 2] <- LPeriod[nL+1, 2]
        LPeriod[nL+1,] <- NA
        LPeak[nL, 1] <- min(LPeak(nL), LPeak(nL+1))
        LPeak[nL+1] <- NA
        LVol[nL] <- LVol[nL+1] + LVol[nL]
        LVol[nL+1] <- NA
      }
    }
  }
  ## % Restore sizes
  ## HFreq(:,1) = []; HPeriod(1,:) = []; nH = nH-1; HPeak(1) = [];
  ## LFreq(:,1) = []; LPeriod(1,:) = []; nL = nL-1; LPeak(1) = [];

  ## % Low pulse count.
  if (nL(1) == 0) {
    ## % No pulses
    FL = zeros(1,5)
    ML = zeros(1,5)
    VL = zeros(1,5)
    DL = zeros(1,5)
  } else {
    ## % Obtain total, mean, min, max, Cv, for each property
    FL = c(
      sum(LFreq),
      mean(LFreq),
      min(LFreq),
      max(LFreq),
      std(LFreq) / mean(LFreq)
    )
    ML = c(
      sum(LPeak),
      mean(LPeak),
      min(LPeak),
      max(LPeak),
      std(LPeak) / mean(LPeak)
    )
    VL = c(
      sum(LVol),
      mean(LVol),
      min(LVol),
      max(LVol),
      std(LVol) / mean(LVol)
    )
    DL = c(
      sum(LPeriod[,2] - LPeriod[,1]),
      mean(LPeriod[,2] - LPeriod[,1]),
      min(LPeriod[,2] - LPeriod[,1]),
      max(LPeriod[,2] - LPeriod[,1]),
      std(LPeriod[,2] - LPeriod[,1]) / mean(LPeriod[,2] - LPeriod[,1])
    )
  }

  ## High pulse count.
  if (nH(1) == 0) {
    ## % No pulses
    FH = zeros(1,5)
    MH = zeros(1,5)
    VH = zeros(1,5)
    DH = zeros(1,5)
  } else {
    ## % Obtain total, mean, min, max, Cv, for each property
    FH = c(
      sum(HFreq),
      mean(HFreq),
      min(HFreq),
      max(HFreq),
      std(HFreq) / mean(HFreq)
    )
    MH = c(
      sum(HPeak),
      mean(HPeak),
      min(HPeak),
      max(HPeak),
      std(HPeak) / mean(HPeak)
    )
    VH = c(
      sum(HVol),
      mean(HVol),
      min(HVol),
      max(HVol),
      std(HVol) / mean(HVol)
    )
    DH = c(
      sum(HPeriod[,2] - HPeriod[,1]),
      mean(HPeriod[,2] - HPeriod[,1]),
      min(HPeriod[,2] - HPeriod[,1]),
      max(HPeriod[,2] - HPeriod[,1]),
      std(HPeriod[,2] - HPeriod[,1]) / mean(HPeriod[,2] - HPeriod[,1])
    )
  }
  TL <- DH[3]
  TH <- DL[3]
  ## ## % Restore vector.
  ## Date(end) = [];
  ## Date = datetime(Date,'ConvertFrom','datenum');
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

## idc <- function(Date, P) {
##   ## %iMHEA Calculation of Maximum Intensity-Duration Curve.
##   ## % [IDC,iM15m,iM1hr] = iMHEA_IDC(Date,P,flag).
##   ## %
##   ## % Input:
##   ## % Date = dd/mm/yyyy hh:mm:ss [date format].
##   ## % P    = Precipitation [mm].
##   ## % flag = leave empty NOT to graph plots.
##   ## %
##   ## % Output:
##   ## % IDC   = Maximum Intensity - Duration Curve [mm/h v time].
##   ## % iM15m = Maximum precipitation intensity (15 min scale) [mm/h].
##   ## % iM1hr = Maximum precipitation intensity (1 hour scale) [mm/h].
##   ## %
##   ## % Boris Ochoa Tocachi
##   ## % Imperial College London
##   ## % Created in July, 2015
##   ## % Last edited in November, 2017

##   ## % Maximum Intensity - Duration Curve.
##   ## h = waitbar(0,'Calculating IDC...');
##   ## % Consider periods only when data exists.
##   VP = P
##   ## Check if the measurements have 5 min interval
##   if (round(median(diff(Date))) != 300) {
##     ## Consider periods only when data exists
##     VDate = Date[!is.na(VP)]
##     VP = VP[!is.na(VP)]
##     VDate = VDate[!VP == 0]
##     VP = VP[!VP == 0]
##     ## Aggregate data to 5 min interval [iMHEA standard?]
##     ## [~,VP] = iMHEA_Aggregation(VDate,VP,5);
##     aggregation(VDate, VP, 5) # TODO 5 mins
##   } else {
##     ## % Consider periods only when data exists.
##     VP[is.na(VP)] = 0
##   }
##   k1 = length(VP)
##   ## % Durations: 5, 10, 15, 30, 60 min; 2, 4, 12, 24 hours; 2 days.
##   D = c(1, 2, 3, 6, 12, 24, 48, 144, 288, 576)
##   u = rep(0, k1)
##   IDC = matrix(data = 0, nrow = length(D), ncol = 2)
##   IDC[,1] = D * 5 # TODO
##   ## Maximum intensities
##   for (i in 1:length(D)) {
##     ## % Define initial IntP(1).
##     u[1] = sum(VP[1:D[i]]) # The sum of the first i elements
##     ## Sums i elements using a moving window
##     for (j in 2:(k1 - D[i] + 1)) {
##       u[j] = u[j-1] + VP[j + D[i] - 1] - VP[j-1]
##     }
##     IDC[i, 2] = max(u) * 12 / D[i]
##     IDC[i, 3] = mean(u[u > 1e-12]) * 12 / D[i]
##     IDC[i, 4] = median(u[u > 1e-12]) * 12 / D[i]
##     ## IDC(i,4) = median(u(u>1E-12),'omitnan')*12/D(i);
##   }

##   ## % Intensity indices.
##   iM15m = IDC[D == 3, 2]  # 5 * 3 = 15 min
##   iM1hr = IDC[D == 12, 2] # 5 * 12 = 60 min = 1h
##   ## TODO plots
## }
