#' Aggregate rainfall data
#'
#' Aggregate rainfall data with cubic spline interpolation.
#'
#' @param Date1 rain_gauge.
#' @param P1 rain_gauge.
#' @param Date2 rain_gauge.
#' @param P2 rain_gauge.
#' @param cutend logical.
#' @param ... Additional arguments.
#'
#' @return tsibble
fill_gaps <- function(Date1,
                      P1,
                      Date2,
                      P2,
                      cutend = FALSE,
                      ...) {

  ## Check if data have the same temporal resolution
  scale1 = median(diff(Date1))
  scale2 = median(diff(Date2))
  if (scale1 != scale2) {
    scale <- max(scale1, scale2)
    x1 <- aggregation(Date1, P1, scale) # TODO
    x2 <- aggregation(Date2, P2, scale) # TODO
    Date1 <- x1$Date
    P1 <- x1$Prec
    Date2 <- x2$Date
    P2 <- x2$Prec
  } else {
    scale = round(scale1)
  }

  ## Define initial and end dates and create single vector
  DI = min(Date1[1], Date2[1])
  DF = max(rev(Date1)[1], rev(Date2)[1])
  NewDate = seq(DI, DF, by = "1 min")
  ## Assign data when they correspond
  NewP1 = rep(NA, length(NewDate))
  NewP2 = rep(NA, length(NewDate))
  NewP1[match(Date1, NewDate)] = P1 # FIXME what if they don't match?
  NewP2[match(Date2, NewDate)] = P2 # FIXME as above
  ## Optionally, cut vectors not to fill gaps after then end
  if (cutend) {
    ## Identify the last non-NA data in both vectors
    indexnP1 = max(which(!is.na(P1)))
    indexnP2 = max(which(!is.na(P2))) # FIXME what if all NA values?
    indexndate = min(Date1[indexnP1], Date2[indexnP2])
    ## Cut vectors after the minimum of the identified dates
    NewP1 = NewP1[NewDate <= indexndate]
    NewP2 = NewP2[NewDate <= indexndate]
    NewDate = NewDate[NewDate <= indexndate]
  }

  ## TEST IF OVERLAPPING DATA EXIST
  ## Extract all the sections where NaN data exist in any of the vectors.
  auxP1 = NewP1
  auxP2 = NewP2
  auxP1 = auxP1[!(is.na(NewP1) | is.na(NewP2))]
  auxP2 = auxP2[!(is.na(NewP1) | is.na(NewP2))]
  ## Check if any of the vectors are empty
  if (length(auxP1) <= 1) {
    ## Restore data if cut before
    if (cutend) {
      NewDate = seq(DI, DF, by = "1 min")
      ## Assign data when they correspond
      NewP1 = rep(NA, length(NewDate))
      NewP2 = rep(NA, length(NewDate))
      NewP1[match(Date1, NewDate)] = P1
      NewP2[match(Date2, NewDate)] = P2
    }
    return(data.frame(NewDate, NewP1, NewP2))
  }
  ## Fill data gaps
  auxCumP1 = cumsum(auxP1)
  auxCumP2 = cumsum(auxP2)
  mod = lm(auxCumP1, aux)
  r2 = summary(mod)$r.squared
  ## Fill gaps only if the correlation is almost perfect
  if (R < 0.99) {
    if (cutend) {
      NewDate = seq(DI, DF, sep = "1 min")
      ## Assign data when they correspond
      NewP1 = rep(NA, length(NewDate))
      NewP2 = rep(NA, length(NewDate))
      NewP1[match(Date1, NewDate)] = P1
      NewP2[match(Date2, NewDate)] = P2
    }
    return(data.frame(NewDate, NewP1, NewP2))
  }
  ## FIXME - what is M?
  ## NewP1(isnan(NewP1)) = NewP2(isnan(NewP1))/M;
  ## NewP2(isnan(NewP2)) = NewP1(isnan(NewP2))*M;
  if (cutend) {
    ## % Assign data when they correspond
    NewP1[match(Date1, NewDate)] = P1
    NewP2[match(Date2, NewDate)] = P2
  }
  NewDate1 = data.frame(NewDate, NewP1, NewP2)
}

average <- function(Date, Q, scale) {
  ## %iMHEA Agregation of hydrological data (average within an interval).
  ## % [NewDate,NewQ,CumQ,VoidQ,MeanQ,MaxQ,MinQ] =
  ## % iMHEA_Average(Date,Q,scale,flag) averages discharge data.
  ## %
  ## % Input:
  ## % Date  = dd/mm/yyyy hh:mm:ss [date format].
  ## % Q     = Stage or Discharge [l/s, m3/s, mm].
  ## % scale = Agregation interval [min].
  ## % flag  = leave empty NOT to run the data voids assessment and plots.
  ## %
  ## % Output:
  ## % NewDate   = dd/mm/yyyy hh:mm:ss [date format] at specified interval.
  ## % NewQ      = Average stage or discharge [l/s, m3/s, mm].
  ## % CumQ      = Cumulative discharge [l/s, m3/s, mm].
  ## % VoidP     = Void intervals [l/s, m3/s, mm].
  ## % MeanQ     = Mean value for specified interval [l/s, m3/s, mm].
  ## % MaxQ      = Maximum value for specified interval [l/s, m3/s, mm].
  ## % MinQ      = Minimum value for specified interval [l/s, m3/s, mm].
  ## %
  ## % Boris Ochoa Tocachi
  ## % Imperial College London
  ## % Created in May, 2014
  ## % Last edited in November, 2017
  Date = Date - seconds(0.25)
  Voids = identify_voids(Date, Q)
  nd = 1440 / scale
  DI = ceiling_date(min(Date)) # Initial date
  DF = ceiling_date(max(Date)) # Final date
  NewDate = seq(DI, DF, by = "1 min")
  n = length(NewDate) # Number of intervals
  NewQ = rep(0, length(NewDate)) # Initialize aggregation
  Date = Date[!is.na(Q)]
  Q = Q[!is.na(Q)]
  k = length(Q) # Length of input data
  ## Set initial counter
  if (Date[1] == NewDate[1]) {
    j = 2
    NewQ[1] = Q[1]
  } else {
    j = 1
  }
  for (i in j:n) {
    l = 0 # Interval data counter
    ## Aggregate values
    ## while j<=k && nd*Date(j)<=NewDate(i) % && nd*Date(j)>NewDate(i-1)
    while (j <= k && Date[j] <= NewDate[i]) {
      NewQ[i] = NewQ[i] + Q[j]
      j = j+1
      l = l+1
    }
    NewQ[i] = NewQ[i] / l
  }
  ## Fill gaps between data when there is only one value missing
  for (i in 2:(n-1)) {
    if (is.na(NewQ[i])) {
      NewQ[i] = mean(c(NewQ[i-1], NewQ[i+1]))
    }
  }
  ## Fill remaining gaps with zeros to calculate cumQ
  NewQ[is.na(NewQ)] = 0

  ## PREPARE THE DATA VECTORS AT THE SPECIFIED SCALE
  CumQ = cumsum(NewQ) # Initialize accumulation
  VoidQ = NewQ
  for (i in 1:length(Voids)) {
    CumQ[NewDate > Voids[i,1] & NewDate < Voids[i,2]] = NA
    NewQ[NewDate > Voids[i,1] & NewDate < Voids[i,2]] = NA
  }
  VoidQ[!is.na(NewQ)] = NA
  ## Check initial and final values of Q for data existence
  if (NewQ[1] == 0 & NewQ[2] != 0) {
    VoidQ[1] = NewQ[1]
    NewQ[1] = NA
    CumQ[1] = NA
  }
  if (rev(NewQ)[1] == 0 && rev(NewQ)[2] != 0) {
    VoidQ[length(VoidQ)] = rev(NewQ)[1]
    NewQ[length(NewQ)] = NA
    CumQ[length(CumQ)] = NA
  }
  MeanQ = mean(NewQ, na.rm = TRUE)
  MaxQ = max(NewQ, na.rm = TRUE)
  MinQ = min(NewQ, na.rm = TRUE)
  return(data.frame(NewDate, NewQ, CumQ, VoidQ))
}
## %% CHECK IF DATA ARE AT THE SAME TEMPORAL RESOLUTION
## scale1 = diff(datenum(Date1))*1440;
## scale2 = diff(datenum(Date2))*1440;
## if nanmedian(scale1) > nanmedian(scale2)
##     fprintf('Input data are not at the same temporal scale.\n')
##     scale = round(nanmedian(scale1)); % Same temporal resolution
##     [Date1,P1] = iMHEA_Aggregation(Date1,P1,scale);
##     [Date2,P2] = iMHEA_Aggregation(Date2,P2,scale);
## elseif nanmedian(scale2) > nanmedian(scale1)
##     fprintf('Input data are not at the same temporal scale.\n')
##     scale = round(nanmedian(scale2)); % Same temporal resolution
##     [Date1,P1] = iMHEA_Aggregation(Date1,P1,scale);
##     [Date2,P2] = iMHEA_Aggregation(Date2,P2,scale);
## else
##     scale = round(nanmedian(scale1)); % Same temporal resolution
## end

## %% VOID ASSESSMENT
## % Run data gap assessment and print inventory.
## fprintf('Data gap assessment of P1.\n')
## [~] = iMHEA_Voids(Date1,P1,1);
## fprintf('Data gap assessment of P2.\n')
## [~] = iMHEA_Voids(Date2,P2,1);

## %% CREATE UNIFIED DATE VECTOR AND ASSIGN CORRESPONDENT INPUT DATA
## % Numeric value of 1 minute: 1/1440
## nd = 1440/scale; % Number of intervals per day
## % Convert Dates to integers to avoid precision errors
## Date1 = round(nd*datenum(Date1));
## Date2 = round(nd*datenum(Date2));
## % Define initial and end dates and create single vector
## DI = min(Date1(1),Date2(1));
## DF = max(Date1(end),Date2(end));
## NewDate = (DI:DF)';
## % Assign data when they correspond
## NewP1 = nan(size(NewDate));
## NewP1(ismember(NewDate,Date1)) = P1;
## NewP2 = nan(size(NewDate));
## NewP2(ismember(NewDate,Date2)) = P2;
## % Optionally, cut vectors not to fill gaps after the end.
## if cutend
##     % Identify the last non-NaN data in both vectors.
##     indexnP1 = find(~isnan(P1),1,'last');
##     indexnP2 = find(~isnan(P2),1,'last');
##     indexndate = min(Date1(indexnP1),Date2(indexnP2));
##     % Cut vectors after the minimum of the identified dates.
##     NewP1(NewDate>indexndate) = [];
##     NewP2(NewDate>indexndate) = [];
##     NewDate(NewDate>indexndate) = [];
## end

## %% TEST IF OVERLAPPING DATA EXIST
## % Extract all the sections where NaN data exist in any of the vectors.
## auxP1 = NewP1; auxP2 = NewP2;
## auxP1(isnan(NewP1)|isnan(NewP2)) = [];
## auxP2(isnan(NewP1)|isnan(NewP2)) = [];
## % Check if any of the vectors is empty
## if isempty(auxP1) || length(auxP1) == 1
##     fprintf('There is not date coincidence between the input data.\n')
##     fprintf('\n')
##     % Restore data if cut before
##     if cutend
##         NewDate = (DI:DF)';
##         % Assign data when they correspond
##         NewP1 = nan(size(NewDate));
##         NewP1(ismember(NewDate,Date1)) = P1;
##         NewP2 = nan(size(NewDate));
##         NewP2(ismember(NewDate,Date2)) = P2;
##     end
##     % Produce outputs
##     if nargout == 1
##         NewDate1 = [NewDate/nd,NewP1,NewP2];
##     else
##         % Otherwise transform dates to date format
##         NewDate1 = datetime(NewDate/nd,'ConvertFrom','datenum');
##         NewDate2 = datetime(NewDate/nd,'ConvertFrom','datenum');
##     end
##     iMHEA_Plot3(datetime(NewDate/nd,'ConvertFrom','datenum'),NewP1,NewP2)
##     return
## end

## %% FILL DATA GAPS
## % Calculate cumulative rainfall curves.
## auxCumP1 = cumsum(auxP1);
## auxCumP2 = cumsum(auxP2);
## [R,M,~] = regression(auxCumP1',auxCumP2');
## % Fill gaps only if the correlation is almost perfect.
## if R < 0.99
##     fprintf('The correlation is not significant as to fill the data, with R2 = %6.4f.',R)
##     fprintf('\n')
##     figure
##     plotregression(auxCumP1,auxCumP2,'Regression')
##     % Restore data if cut before
##     if cutend
##         NewDate = (DI:DF)';
##         % Assign data when they correspond
##         NewP1 = nan(size(NewDate));
##         NewP1(ismember(NewDate,Date1)) = P1;
##         NewP2 = nan(size(NewDate));
##         NewP2(ismember(NewDate,Date2)) = P2;
##     end
##     % Produce outputs
##     if nargout == 1
##         NewDate1 = [NewDate/nd,NewP1,NewP2];
##         % NewDate1 = [NewDate1,NewP1,NewP2,cumsum(NewP1),cumsum(NewP2)];
##     else
##         % Otherwise transform dates to date format
##         NewDate1 = datetime(NewDate/nd,'ConvertFrom','datenum');
##         NewDate2 = datetime(NewDate/nd,'ConvertFrom','datenum');
##     end
##     return
## end
## if nargin >= 6
##     % Plot the regression.
##     figure
##     plotregression(auxCumP1,auxCumP2,'Regression')
## end
## NewP1(isnan(NewP1)) = NewP2(isnan(NewP1))/M;
## NewP2(isnan(NewP2)) = NewP1(isnan(NewP2))*M;

## %% RESTORE THE DATA AND THE END OF THE VECTORS
## if cutend
##     % Assign data when they correspond
##     NewP1(ismember(NewDate,Date1)) = P1;
##     NewP2(ismember(NewDate,Date2)) = P2;
## end

## %% GENERATE OUTPUTS
## % Restore Dates from integers made to avoid precision errors
## Date1 = datetime(Date1/nd,'ConvertFrom','datenum');
## Date2 = datetime(Date2/nd,'ConvertFrom','datenum');
## NewDate1 = datetime(NewDate/nd,'ConvertFrom','datenum');
## NewDate2 = datetime(NewDate/nd,'ConvertFrom','datenum');

## %% PRINT RESULTS
## fprintf('Rainfall volumes before filling gaps: %8.2f and %8.2f mm.\n',nansum(P1),nansum(P2))
## fprintf('Rainfall volumes after filling gaps: %8.2f and %8.2f mm.\n',nansum(NewP1),nansum(NewP2))
## fprintf('\n')

## % Plot regressions
## if nargin >= 6
##     % Extract all the sections where NaN data exists in any of the vectors.
##     auxP1 = NewP1; auxP2 = NewP2;
##     auxP1(isnan(NewP1)|isnan(NewP2)) = [];
##     auxP2(isnan(NewP1)|isnan(NewP2)) = [];
##     % Calculate cumulative rainfall curves.
##     auxCumP1 = cumsum(auxP1);
##     auxCumP2 = cumsum(auxP2);
##     % Plot the results
##     figure
##     plotregression(auxCumP1,auxCumP2,'Regression')

##     % Plot time series
##     figure
##     subplot(2,1,1)
##     plot(Date1,P1,Date2,P2)
##     xlabel('Date')
##     ylabel('Precipitation [mm]')
##     title('Before gap fill')
##     box on
##     subplot(2,1,2)
##     plot(NewDate1,NewP1,NewDate2,NewP2)
##     xlabel('Date')
##     ylabel('Precipitation [mm]')
##     title('After gap fill')
##     box on
## end

## if nargout == 1
##     NewDate1 = [NewDate/nd,NewP1,NewP2];
## end
}
