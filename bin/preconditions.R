#Explore the preconditions of drought events

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_short_met1.Rds')
ma_ave <- 30 # time period for the estimation of mean (years)

test <- dta[PT_ID == 493] 
tp <- as.Date("2015-01-01")
dur <- 5 * 12 - 1#duration for the estimation of the cumulative sum of the given variables (months)
test[, nPET := scale(pet)]
test[, nP := scale(p)] #original nP is standardized by month, here are standardized by ts mean
test[, nQ := scale(q)]
test[, nS := scale(s)]

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
  ylab(label = 'Soil moisture (z-score)') + 
  scale_fill_manual(values = var_cols) +
  theme_bw() +
  theme_opts

####
#working with events

pre <- 12
aft <- 12

events <- unique( test[dur > 15, .(as.Date(min(DTM)), dur), ID])
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

def_vols <- melt(test[, .(DTM, p_nV, q_nV, s_nV)], id.vars = 'DTM')
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
#working with many points

pre <- 12
aft <- 12
event_dur <- 6
my_events <- c(1921, 1976, 2003, 2012, 2015, 2017)
grid_cell_thres <- 10

test <- dta[REG == 'NEU'] 
test[, nPET := scale(pet), PT_ID]
test[, nP := scale(p), PT_ID] 
test[, nQ := scale(q), PT_ID]
test[, nS := scale(s), PT_ID]
test[, yr := as.numeric(year(DTM))]

events <- unique(test[dur >= event_dur & year(start) %in% my_events, .(start, dur, PT_ID), ID])
events[, end := start + months(dur)]

no_events_start <- events[, .N, start]
no_events_end <- events[, .N, end]
setorder(no_events_start, start)
setorder(no_events_end, end)

events <- events[start %in% no_events_start[N > grid_cell_thres]$start]
events <- events[end %in% no_events_end[N > grid_cell_thres]$end]

test_anom <- rbind( #investigating the preconditions of the events
  cbind(cumsum_events_space(test, 'nP', events, pre, aft), var = 'nP'), 
  cbind(cumsum_events_space(test, 'nPET', events, pre, aft), var = 'nPET'),
  cbind(cumsum_events_space(test, 'nQ', events, pre, aft), var = 'nQ'),
  cbind(cumsum_events_space(test, 'nS', events, pre, aft), var = 'nS')) 
test_anom[, cumsum := cumsum(anomaly), .(var, PT_ID, PT_ID)]

test_anom_m <- test_anom[, .(median(cumsum), .N), .(var, DTM, event)]
colnames(test_anom_m)[4] <- 'cumsum'
test_anom_m <- test_anom_m[complete.cases(test_anom_m)]
test_anom_m <- test_anom_m[N >= grid_cell_thres]

def_vols <- melt(test[, .(DTM, PT_ID, p_nV, q_nV, s_nV)], id.vars = c('DTM', 'PT_ID'))
def_vols <- def_vols[complete.cases(def_vols)]
def_vols <- unique(merge(test_anom[, .(DTM, PT_ID = as.numeric(PT_ID), event)], def_vols, by = c('DTM', 'PT_ID')))
def_vols <- def_vols[, .(value = mean(value)), .(DTM, variable, event)]
  
rects <- data.frame(DTM = events$start, 
                    DTM_end = events$start + months(events$dur))
rects <- unique(merge(rects, test_anom_m[, .(DTM, event)], by = 'DTM'))
ggplot(test_anom_m, aes(x = as.Date(DTM), y = cumsum, col = var)) +
  geom_rect(data = rects, aes(xmin = DTM, xmax = DTM_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.05, fill = 'grey50', inherit.aes = F) +
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
  scale_x_date(breaks = date_breaks("6 months"), 
               labels = date_format("%b %Y")) +
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




