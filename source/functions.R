library(data.table); library(ggplot2); library(lubridate); library(zoo); library(scales) 
library(ncdf4)

get_anomaly <- function(dataset, var, event_start, mwin, duration){  #mwin in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = mwin, na.pad = T, align = 'right')]
  event_pr_mean <- dataset[DTM == event_start, roll_mean]
  out <- dataset[as.Date(DTM) >= event_start & as.Date(DTM) < event_start  %m+% months(duration), .(DTM, anomaly = eval(parse(text = var)) - event_pr_mean)]  #perhaps consider monthly means as well
  return(out)
} # Calculates the difference between a *var* and its average in the previous *mwin* years for each value of the next *duration* months

get_periods <- function(eve, pre, aft){
  oo <- foreach(i = 1:length(eve[, unique(ID)]), .combine = 'rbind') %do% {
    data.table(no = i, 
               DTM = seq(eve[, min(DTM) - months(pre), ID]$V1[i],
                         eve[, max(DTM) + months(aft), ID]$V1[i], 'month'))
  }
  event <- eve[, unique(year(start))]
  event <- data.table(cbind(event, no = 1:length(event)))
  oo <- oo[event, on = 'no']
  oo[, no := NULL]
  return(oo)
}

cumsum_events <-  function(dataset, eve, pre, aft, scale = T){
  oo <- get_periods(eve, pre, aft)
  out <- dataset[oo, on = 'DTM']
  if(scale == T) (out[, cumsum := cumsum(anom_z), event])
  else (out[, cumsum := cumsum(anom), event])
  return(out)
}




get_anomaly_events <- function(dataset, var, event_start, mwin, duration){  #mwin in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = mwin, na.pad = T, align = 'right')]
  event_pr_mean <- dataset[DTM %in% event_start, roll_mean]
  event_pr_mean <- event_pr_mean[complete.cases(event_pr_mean)]
  out <- list()
  for(i in 1:length(event_pr_mean)){
  out[[i]] <- dataset[as.Date(DTM) >= event_start[i] & as.Date(DTM) < event_start[i] %m+% months(duration[i]), 
                 .(DTM, anomaly = eval(parse(text = var)) - event_pr_mean[i])]
  }
  names(out) <- year(event_start)
  out <- data.table(melt(out, id.vars = c('DTM', 'anomaly'),  varnames = 'event'))
  colnames(out)[3] <- 'event'
  out[, event := as.numeric(event)]
  return(out)
} 

cumsum_events <- function(dataset, var, event_start, pre_dur, event_dur, aft_dur, mwin = 30){  #mwin in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = mwin, na.pad = T, align = 'right')]
  event_pr_mean <- dataset[DTM %in% event_start, roll_mean]
  out <- list()
  for(i in 1:length(event_pr_mean)){
    out[[i]] <- dataset[as.Date(DTM) >= (event_start[i] - months(pre_dur)) & 
                          as.Date(DTM) <= (event_start[i] + months(event_dur[i] + aft_dur)), 
                        .(DTM, anomaly = eval(parse(text = var)) - event_pr_mean[i])]
  }
  names(out) <- year(event_start)
  out <- data.table(melt(out, id.vars = c('DTM', 'anomaly'),  varnames = 'event'))
  colnames(out)[3] <- 'event'
  out[, event := as.numeric(event) + floor(pre_dur / 12)]
  return(out)
} 

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


add_prv_yr <- function(drought, dataset = dta){
  aa <- unique(drought[, .(PT_ID, yr)]) 
  aa_prv <- aa
  aa$event = factor('cur_yr')
  aa_prv$event = factor('prv_yr')
  aa_prv[, yr := yr - 1] 
  aa <- rbind(aa, aa_prv)
  out <- merge(aa, dataset)
  return(out)
}

add_nxt_yr <- function(drought, dataset = dta){
  aa <- unique(drought[, .(PT_ID, yr)]) 
  aa_nxt <- aa
  aa$event = factor('cur_yr')
  aa_nxt$event = factor('nxt_yr')
  aa_nxt[, yr := yr + 1] 
  aa <- rbind(aa, aa_nxt)
  out <- merge(aa, dataset)
  return(out)
}
