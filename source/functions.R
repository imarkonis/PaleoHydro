library(data.table); library(ggplot2); library(lubridate); library(zoo); library(scales) 
library(ncdf4); library(doSNOW)

get_anomaly <- function(dataset, var, event_start, mwin, duration){  #mwin in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = mwin, na.pad = T, align = 'right')]
  event_pr_mean <- dataset[DTM == event_start, roll_mean]
  out <- dataset[as.Date(DTM) >= event_start & as.Date(DTM) < event_start  %m+% months(duration), .(DTM, anomaly = eval(parse(text = var)) - event_pr_mean)]  #perhaps consider monthly means as well
  return(out)
} # Calculates the difference between a *var* and its average in the previous *mwin* years for each value of the next *duration* months

get_periods <- function(dataset, eve, pre, aft){
  events <- unique(eve[, .(PT_ID, start, dur, event = year(start))])
  events[, start_pre := start - months(pre)]
  events[, start_aft := start + months(dur) + months(aft)]
  out <- foreach(i = 1:nrow(events), .combine = 'rbind') %do% {
    unique(dataset[PT_ID == events$PT_ID[i] &
                     DTM >= events$start_pre[i] & 
                     DTM <= events$start_aft[i],
                   .(PT_ID, DTM, event = events$event[i])])
  }
  return(out)
}

get_periods_par <- function(dataset, eve, pre, aft){
  no_cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS')) - 1
  if(no_cores < 1 | is.na(no_cores))(no_cores <- 1)
  cluster = makeCluster(no_cores, type = "SOCK")
  registerDoSNOW(cluster)
  
  events <- unique(eve[, .(PT_ID, start, dur, event = year(start))])
  events[, start_pre := start - months(pre)]
  events[, start_aft := start + months(dur) + months(aft)]
  out <- foreach(i = 1:nrow(events), .combine = 'rbind', .packages = c('data.table', 'lubridate')) %dopar% {
    unique(dataset[PT_ID == events$PT_ID[i] &
                     DTM >= events$start_pre[i] & 
                     DTM <= events$start_aft[i],
                   .(PT_ID, DTM, event = events$event[i])])
  }
  stopCluster(cluster)
  return(out)
}

cumsum_events <-  function(dataset, eve, pre, aft, scale = T, par = F){
  if(par == F) (oo <- get_periods(dataset, eve, pre, aft))
  else(oo <- get_periods_par(dataset, eve, pre, aft))
  out = dataset[oo, on = c('DTM', 'PT_ID')]
  if(scale == T) (out[, cumsum := cumsum(anom_z), c('event', 'variable', 'PT_ID')])
  else (out[, cumsum := cumsum(anom), c('event', 'variable', 'PT_ID')])
  return(out)
}

space_prpg <- function(event, thres = 0.1, same_year = F){ #Spatial propagation of drought event derived from maximum number of points
  peak <- as.Date(apply(table(event[!is.na(value) & !is.na(start), DTM, variable]), 1, function(x) names(which.max(x))))
  peak_pts <- unique(event[DTM %in% peak, PT_ID])
  start <- event[!is.na(value) & !is.na(start), .N, .(variable, start)]
  start[, limit := thres * sum(N), .(variable)]
  if(same_year == F){
    start <- start[N > limit, min(start), variable]}
  else (start <- start[N > limit & year(start) == year(min(event$DTM)), min(start), variable])
  end <- event[!is.na(value) & !is.na(start), .N, .(variable, end = start + months(dur))]
  end[, limit := thres * sum(N), .(variable)]
  end <- end[N > limit, max(end), variable]
  out <- data.frame(start = start$V1, peak, end = end$V1)
  rownames(out) <- c('p_dv', 'q_dv', 's_dv')
  return(out)
}


event_start <- function(dataset, eve){
  return(table(dataset[event == eve & variable == 'p', min(DTM), PT_ID]$V1))
}

event_end <- function(dataset, eve){
  return(table(dataset[event == eve & variable == 'p', max(DTM), PT_ID]$V1))
}




#############OLDER
cumsum_events_space <- function(dataset, var, events_dt, pre_dur, aft_dur, mwin = 30){  #mwin in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = mwin, na.pad = T, align = 'right'), PT_ID]
  event_pr_mean <- dataset[DTM == start, roll_mean, .(PT_ID, DTM)]
  event_pr_mean <- event_pr_mean[complete.cases(event_pr_mean)]
  out <- list()
  for(i in 1:length(events_dt$PT_ID)){
    out[[i]] <- dataset[PT_ID == events_dt$PT_ID[i] & 
                          DTM >= events_dt$start[i] - months(pre_dur) & 
                          DTM <= events_dt$start[i] + months(events_dt$dur[i] + aft_dur), 
                        .(DTM, 
                          anomaly = eval(parse(text = var)) - event_pr_mean[i]$roll_mean, 
                          event = year(events_dt$start[i]))]
  }
  names(out) <- events_dt$PT_ID
  out <- data.table(melt(out, id.vars = c('DTM', 'anomaly', 'event')))
  colnames(out)[4] <- 'PT_ID'
  return(out)
} 

cumsum_events_space_all <- function(dataset, var, events_dt, pre_dur, aft_dur, mwin = 30){  #mwin in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = mwin, na.pad = T, align = 'right'), PT_ID]
  event_pr_mean <- dataset[DTM == start, roll_mean, .(PT_ID, DTM)]
  event_pr_mean <- event_pr_mean[complete.cases(event_pr_mean)]
  out <- list()
  for(i in 1:length(events_dt$PT_ID)){
    out[[i]] <- dataset[PT_ID == events_dt$PT_ID[i] & 
                          DTM >= events_dt$min_start[i] & 
                          DTM <= events_dt$max_end[i], 
                        .(DTM, 
                          anomaly = eval(parse(text = var)) - event_pr_mean[i]$roll_mean, 
                          event = year(events_dt$start[i]))]
  }
  names(out) <- events_dt$PT_ID
  out <- data.table(melt(out, id.vars = c('DTM', 'anomaly', 'event')))
  colnames(out)[4] <- 'PT_ID'
  return(out)
} 

