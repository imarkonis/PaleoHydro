#Explore the preconditions of drought events

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_all.Rds')

test <- dta[x == 50 & y == 20,] #CR or close
tp <- as.Date("2010-12-01")
ma_ave <- 30 # time period for the estimation of mean (years)
dur <- 5 * 12 #duration for the estimation of the cumulative sum of the given variables (months)
test[, nPET := scale(aPET)]
test[, nP := scale(aP)] #original nP is standardized by month, here are standardized by ts mean
test[, nQ := scale(aQ)]
test[, nS := scale(aS)]

test_anom <- rbind(
  cbind(get_anomaly(test, 'nP', tp, ma_ave, dur), var = 'nP'),
  cbind(get_anomaly(test, 'nPET', tp, ma_ave, dur), var = 'nPET'),
  cbind(get_anomaly(test, 'nQ', tp, ma_ave, dur), var = 'nQ'),
  cbind(get_anomaly(test, 'nS', tp, ma_ave, dur), var = 'nS')) 
  
test_anom[, cum_anom := cumsum(anomaly), var]
test_anom[, month := factor(month(DTM))]
test_anom[, year := factor(year(DTM))]

ggplot(test_anom, aes(x = DTM, y = cum_anom, col = var)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1, 2, 5, 3)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  theme_bw() +
  theme_opts

ggplot(test_anom[var == 'nS'], aes(x = month, y = anomaly, fill = year)) +
  geom_col(col = 'black', size = 0.75) +
  geom_hline(yintercept = 0) +
  xlab(label = "Month") +
  ylab(label = 'Cumulative S (z-score)') + 
  scale_fill_manual(values = var_cols) +
  theme_bw() +
  theme_opts

####

test <- dta[x == 30 & y == 30,] #Some other place (south) where the effect of p/pet to q/s is very clear
tp <- as.Date("2010-12-01")
test[, nPET := scale(aPET)] 
test[, nP := scale(aP)] 
test[, nQ := scale(aQ)]
test[, nS := scale(aS)]

test_anom <- rbind(
  cbind(get_anomaly(test, 'nP', tp, ma_ave, dur), var = 'nP'), 
  cbind(get_anomaly(test, 'nPET', tp, ma_ave, dur), var = 'nPET'),
  cbind(get_anomaly(test, 'nQ', tp, ma_ave, dur), var = 'nQ'),
  cbind(get_anomaly(test, 'nS', tp, ma_ave, dur), var = 'nS')) 

test_anom[, cum_anom := cumsum(anomaly), var]
test_anom[, month := factor(month(DTM))]
test_anom[, year := factor(year(DTM))]

ggplot(test_anom, aes(x = DTM, y = cum_anom, col = var)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1, 2, 5, 3)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  theme_bw() +
  theme_opts

ggplot(test_anom[var == 'nS'], aes(x = month, y = anomaly, fill = year)) +
  geom_col(col = 'black', size = 0.75) +
  geom_hline(yintercept = 0) +
  xlab(label = "Month") +
  ylab(label = 'Cumulative S (z-score)') + 
  scale_fill_manual(values = var_cols) +
  theme_bw() +
  theme_opts

####

test <- dta[x == 50 & y == 20,] #working with events
test[, nPET := scale(aPET)]
test[, nP := scale(aP)] 
test[, nQ := scale(aQ)]
test[, nS := scale(aS)]

events <- unique(test[dur > 6, ID])
events_start <- test[ID %in% events, as.Date(min(DTM)), ID]
events_start[, yr := year(V1)]
colnames(events_start)[2] <- 'DTM'
dur <- 13

test_anom <- rbind( 
  cbind(get_anomaly_events(test, 'nP', events_start$DTM, ma_ave, dur), var = 'nP'), 
  cbind(get_anomaly_events(test, 'nPET', events_start$DTM, ma_ave, dur), var = 'nPET'),
  cbind(get_anomaly_events(test, 'nQ', events_start$DTM, ma_ave, dur), var = 'nQ'),
  cbind(get_anomaly_events(test, 'nS', events_start$DTM, ma_ave, dur), var = 'nS')) 

test_anom[, cum_anom := cumsum(anomaly), .(var, event)]
test_anom[, month := factor(month(DTM))]
test_anom[, year := factor(year(DTM))]

ggplot(test_anom, aes(x = DTM, y = cum_anom, col = var)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1, 2, 5, 3)]) +
  facet_wrap(vars(event), scales = 'free') +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  theme_bw() +
  theme_opts

test_anom <- rbind( #investigating the preconditions of the events
  cbind(get_preconditions(test, 'nP', events_start$DTM, ma_ave, dur), var = 'nP'), 
  cbind(get_preconditions(test, 'nPET', events_start$DTM, ma_ave, dur), var = 'nPET'),
  cbind(get_preconditions(test, 'nQ', events_start$DTM, ma_ave, dur), var = 'nQ'),
  cbind(get_preconditions(test, 'nS', events_start$DTM, ma_ave, dur), var = 'nS')) 

test_anom[, cum_anom := cumsum(anomaly), .(var, event)]
test_anom[, month := factor(month(DTM))]
test_anom[, year := factor(year(DTM))]

ggplot(test_anom, aes(x = DTM, y = cum_anom, col = var)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1, 2, 5, 3)]) +
  facet_wrap(vars(event), scales = 'free') +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  theme_bw() +
  theme_opts




veg_dr <- readRDS(file = './data/veg_droughts_AMJ_5_8.Rds')

months_before <- 6
veg_dr_2003 <- add_prv_yr(veg_dr)
veg_dr_2003 <- veg_dr_2003[yr == 2003, .(PT_ID, yr, month, event, time, nP, nP3, nQ, nS, nT)] 

abs_start <- unique(veg_dr_2003[month == 3 & event == 'cur_yr', time])
veg_dr_2003_prv <- veg_dr_2003[time < abs_start & time >= (abs_start - months_before),
                       .(PT_ID, time, nP, nP3, nQ, nS, nT)]

to_plot <- melt(data = veg_dr_2003_prv, id.vars = c('PT_ID', 'time'))
to_plot[, time := time - abs_start]
plot_var_dens_prv(to_plot)




