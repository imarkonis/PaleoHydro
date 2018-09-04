#Explore the preconditions of drought events

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_all.Rds')
ma_ave <- 30 # time period for the estimation of mean (years)

test <- dta[x == 50 & y == 20, ] #CR or close
tp <- as.Date("2010-12-01")
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

pre <- 12
aft <- 12

events <- unique( test[dur > 9, .(as.Date(min(DTM)), dur), ID])
events[, yr := year(V1)]
colnames(events)[2] <- 'start'

test_anom <- rbind( #investigating the preconditions of the events
  cbind(cumsum_events(test, 'nP', events$start, pre, events$dur, aft), var = 'nP'), 
  cbind(cumsum_events(test, 'nPET', events$start, pre, events$dur, aft), var = 'nPET'),
  cbind(cumsum_events(test, 'nQ', events$start, pre, events$dur, aft), var = 'nQ'),
  cbind(cumsum_events(test, 'nS', events$start, pre, events$dur, aft), var = 'nS')) 

test_anom[, cumsum := cumsum(anomaly), .(var, event)]
test_anom[, month := factor(month(DTM))]
test_anom[, year := factor(year(DTM))]

def_vols <- melt(test[, .(DTM, p, q, s)], id.vars = 'DTM')
def_vols <- def_vols[complete.cases(def_vols)]
def_vols <- merge(test_anom[, .(DTM, event)], def_vols, by = 'DTM')
rects <- data.frame(DTM = events$start, 
                    DTM_end = events$start + months(events$dur))
rects <- unique(merge(rects, test_anom[, .(DTM, event)], by = 'DTM'))
ggplot(test_anom, aes(x = DTM, y = cumsum, col = var)) +
  geom_rect(data = rects, aes(xmin = DTM, xmax = DTM_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.3, fill = "grey70", col= "grey70", inherit.aes = F) +
  geom_bar(data = def_vols, aes(x = DTM, y = value, fill = variable), col = 'black',
           position = "dodge", stat = 'identity',  inherit.aes = F, alpha = 0.3) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) + 
  scale_color_manual(values = var_cols[c(1, 2, 5, 3)]) +
  scale_fill_manual(values = var_cols[c(1, 5, 3)]) +
  facet_wrap(vars(event), scales = 'free_x') +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts

####
test <- dta[x > 50 & x < 55 & y > 20 & y < 25,] #working with more points
test[, nPET := scale(aPET), PT_ID]
test[, nP := scale(aP), PT_ID] 
test[, nQ := scale(aQ), PT_ID]
test[, nS := scale(aS), PT_ID]

pre <- 6
aft <- 3
my_event <- 2012

events <- unique( test[dur >= 12, .(as.Date(min(DTM)), dur), ID])
events[, yr := year(V1)]
colnames(events)[2] <- 'start'

test_anom <- rbind( #investigating the preconditions of the events
  cbind(cumsum_events_space(test, 'nP', events$start, pre, events$dur, aft), var = 'nP'), 
  cbind(cumsum_events_space(test, 'nPET', events$start, pre, events$dur, aft), var = 'nPET'),
  cbind(cumsum_events_space(test, 'nQ', events$start, pre, events$dur, aft), var = 'nQ'),
  cbind(cumsum_events_space(test, 'nS', events$start, pre, events$dur, aft), var = 'nS')) 

test_anom[, cumsum := cumsum(anomaly), .(var, event, PT_ID)]
test_anom[, month := factor(month(DTM))]
test_anom[, year := factor(year(DTM))]

#test_anom <- test_anom[event == my_event]

test_anom_m <- test_anom[, .(median(cumsum), .N), .(var, DTM, event)]
colnames(test_anom_m)[4] <- 'cumsum'
test_anom_m <- test_anom_m[complete.cases(test_anom_m)]
def_vols <- melt(test[, .(DTM, PT_ID, p, q, s)], id.vars = c('DTM', 'PT_ID'))
def_vols <- def_vols[complete.cases(def_vols)]
def_vols <- merge(test_anom[, .(DTM, PT_ID, event)], def_vols, by = c('DTM', 'PT_ID'))
rects <- data.frame(DTM = events$start, 
                    DTM_end = events$start + months(events$dur))
rects <- unique(merge(rects, test_anom[, .(DTM, event)], by = 'DTM'))
ggplot(test_anom_m, aes(x = DTM, y = cumsum, col = var)) +
  geom_rect(data = rects, aes(xmin = DTM, xmax = DTM_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.3, fill = "grey70", col= "grey70", inherit.aes = F) +
  #geom_bar(data = def_vols, aes(x = DTM, y = value, fill = variable), col = 'black',
  #         position = "dodge", stat = 'identity',  inherit.aes = F, alpha = 0.3) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) + 
  scale_color_manual(values = var_cols[c(1, 2, 5, 3)]) +
  scale_fill_manual(values = var_cols[c(1, 5, 3)]) +
  facet_wrap(vars(event), scales = 'free_x') +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
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




