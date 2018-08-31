#Explore the preconditions of drought events

library(data.table); library(ggplot2); library(lubridate); library(zoo)

get_anomaly <- function(dataset, var, event_start, ma_window, duration){  #ma_window in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = ma_window, na.pad = T, align = 'right')]
  event_pr_mean <- dataset[DTM == event_start, roll_mean]
  out <- dataset[as.Date(DTM) > event_start & as.Date(DTM) < event_start  %m+% months(duration), .(DTM, anomaly = eval(parse(text = var)) - event_pr_mean)]  #perhaps consider monthly means as well
  return(out)
} # Calculates the difference between a *var* and its average in the previous *ma_window* years for each value of the next d*uration* years

get_anomaly_events <- function(dataset, var, event_start, ma_window, duration){  #ma_window and duration in years!
  dataset[, roll_mean := rollmean(eval(parse(text = var)), k = ma_window, na.pad = T, align = 'right')]
  event_pr_mean <- dataset[DTM %in% event_start, roll_mean]
  out <- list()
  for(i in 1:length(event_pr_mean)){
  out[[i]] <- dataset[as.Date(DTM) > event_start[i] & as.Date(DTM) < event_start[i] %m+% months(duration), 
                 .(DTM, anomaly = eval(parse(text = var)) - event_pr_mean[i])]
  }
  names(out) <- year(event_start)
  out <- data.table(melt(out, id.vars = c('DTM', 'anomaly'),  varnames = 'event'))
  colnames(out)[3] <- 'event'
  out[, event := as.numeric(event)]
  return(out)
} 

get_preconditions <- function(dataset, var, event_start, ma_window, duration){
  out <- get_anomaly_events(dataset, var, event_start %m-% months(duration), ma_window, duration)
  out[, event := event + floor(duration / 12)]
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
