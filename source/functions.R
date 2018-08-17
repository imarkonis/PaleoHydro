library(data.table); library(ggplot2)

put_prev_aft_yr <- function(drought, dataset = dta){
  veg_spa_time <- unique(drought[, .(PT_ID, x, y, yr)]) 
  veg_spa_time_pr <- veg_spa_time_aft <- veg_spa_time
  veg_spa_time$event = factor('cur_yr')
  veg_spa_time_pr$event = factor('pr_yr')
  veg_spa_time_pr[, yr := yr - 1] 
  veg_spa_time_aft$event = factor('aft_yr')
  veg_spa_time_aft[, yr := yr + 1] 
  veg_spa_time <- rbind(veg_spa_time, veg_spa_time_pr, veg_spa_time_aft)
  veg_dr <- merge(veg_spa_time, dataset)
}