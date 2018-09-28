library(data.table); library(ggplot2); library(lubridate); library(zoo); library(scales) 
library(ncdf4); library(doSNOW); library(reshape2)
get.periods <- function(dataset, eve, pre, aft){
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

get.periods.par <- function(dataset, eve, pre, aft){
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

cumsum.events <-  function(dataset, eve, pre, aft, scale = T, par = F){
  if(par == F) (oo <- get_periods(dataset, eve, pre, aft))
  else(oo <- get_periods_par(dataset, eve, pre, aft))
  out = dataset[oo, on = c('DTM', 'PT_ID')]
  if(scale == T) (out[, cumsum := cumsum(anom_z), c('event', 'variable', 'PT_ID')])
  else (out[, cumsum := cumsum(anom), c('event', 'variable', 'PT_ID')])
  return(out)
}

space.prpg <- function(event, thres = 0.1, same_year = F){ #Spatial propagation of drought event derived from maximum number of points
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
