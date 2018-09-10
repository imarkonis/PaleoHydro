
cumsum_events_space <- function(dataset, var, events_dt, pre_dur, aft_dur, mwin = 30){  #mwin in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = mwin, na.pad = T, align = 'right'), PT_ID]
  event_pr_mean <- dataset[DTM == start, roll_mean, .(PT_ID, DTM)]
  event_pr_mean <- event_pr_mean[complete.cases(event_pr_mean)]
  out <- list()
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)
  out <- foreach (i = 1:length(events_dt$PT_ID), .packages = c("data.table", "lubridate")) %dopar%  {
    dataset[PT_ID == events_dt$PT_ID[i] & as.Date(DTM) >= as.Date(events_dt$start[i]) - months(pre_dur) & 
              as.Date(DTM) <= as.Date(events_dt$start[i]) + months(events_dt$dur[i] + aft_dur), 
            .(DTM, anomaly = eval(parse(text = var)) - event_pr_mean[i]$roll_mean, event = floor(median(yr)))]
  }
  stopCluster(cluster)
  names(out) <- events_dt$PT_ID
  out <- data.table(melt(out, id.vars = c('DTM', 'anomaly', 'event')))
  colnames(out)[4] <- 'PT_ID'
  return(out)
} 


cumsum_events_space <- function(dataset, var, events_dt, pre_dur, aft_dur, mwin = 30){  #mwin in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = mwin, na.pad = T, align = 'right'), PT_ID]
  event_pr_mean <- dataset[DTM == start, roll_mean, .(PT_ID, DTM)]
  event_pr_mean <- event_pr_mean[complete.cases(event_pr_mean)]
  out <- list()
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)
  out <- foreach (i = 1:length(events_dt$PT_ID), .packages = c("lubridate")) %dopar%  {
    dataset[PT_ID == events_dt$PT_ID[i] & 
              as.Date(DTM) >= as.Date(events_dt$start[i]) - months(pre_dur) & 
              as.Date(DTM) <= as.Date(events_dt$start[i]) + months(events_dt$dur[i] + aft_dur), 
            .(DTM, anomaly = eval(parse(text = var)) - event_pr_mean[i]$roll_mean, event = floor(median(yr)))]
  }
  stopCluster(cluster)
  names(out) <- events_dt$PT_ID
  out <- data.table(melt(out, id.vars = c('DTM', 'anomaly', 'event')))
  colnames(out)[4] <- 'PT_ID'
  return(out)
} 