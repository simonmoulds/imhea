imhea_depure <- function(events, times) {
  ## Remove consecutive tips above maximum intensity
  ## Clone of iMHEA_Depure.m
  ##
  nd = 86400
  min_tip_interval = 1.1 / nd # TODO make this a package option
  ## Time between tips
  intervals = int_length(int_diff(times))
  ## Add arbitrary long interval at the beginning of the time series
  intervals = c(min_tip_interval * 100, intervals)
  ## Identify tips separated by less than min_tip_interval
  idx = which(intervals <= min_tip_interval) + 1
  events[idx] = 0.
  ## %% PRINT RESULTS
  ## fprintf('Removing tips occurring faster than MinT = %6.2f seconds.\n',MinT*86400)
  ## fprintf('Number of tips identified: %4i.\n',length(find(EventDiff)))
  ## fprintf('Rainfall volume before depuration: %8.2f mm.\n',nansum(Event_mm))
  ## fprintf('Rainfall volume after depuration: %8.2f mm.\n',nansum(NewEvent_mm))
  ## fprintf('\n')
  events
}
