#Explore the preconditions of drought events

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_short_met1.Rds')
mhm <- dta[, .(PT_ID, DTM, ID, start, dur, p, q, s, pet)]
mhm <- melt(mhm, id.vars = c('PT_ID', 'DTM', 'ID', 'start', 'dur'))
mhm[, anom := value - mean(value, na.rm = T), variable]

mhm_dv <- dta[, .(PT_ID, DTM, ID, start, dur, p_dv, q_dv, s_dv)]
mhm_dv <- melt(mhm_dv, id.vars = c('PT_ID', 'DTM', 'ID', 'start', 'dur'))

pre <- 6
aft <- 12
dur_event <- 9

test <- mhm[PT_ID == 493] 
test[, anom_z := scale(anom), variable]
test_events <- test[dur > dur_event] #1st example
#test_events <- test[year(start) == 1920 & dur > 3] #2nd example
#test_events <- test[year(start) == 2000] #3d example
test_cumsum <- cumsum_events(test, test_events, pre, aft, scale = T, par = T) 

#plotting
rects <- data.frame(DTM = test_events$start, 
                    DTM_end = test_events$start + months(test_events$dur))
rects <- unique(merge(rects, test_cumsum[, .(DTM, event)], by = 'DTM'))

def_vols <- mhm_dv[PT_ID == 493 & DTM %in% test_cumsum$DTM]
def_vols <- def_vols[!is.na(value)]
def_vols <- unique(merge(test_cumsum[, .(DTM, PT_ID = as.numeric(PT_ID), event)], def_vols, by = c('DTM', 'PT_ID')))

ggplot(test_cumsum, aes(x = DTM, y = cumsum, col = variable)) +
  geom_rect(data = rects, aes(xmin = DTM, xmax = DTM_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.3, fill = "grey70", col = "grey70", inherit.aes = F) +
  geom_bar(data = def_vols, aes(x = DTM, y = value, fill = variable), col = 'black',
           position = "dodge", stat = 'identity',  inherit.aes = F, alpha = 0.6) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) + 
  scale_fill_manual(values = var_cols[c(1, 5, 3)]) +
  scale_color_manual(values = var_cols[c(1, 5, 3, 2)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  facet_wrap(vars(event), scales = 'free_x') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts

#many points

test <- mhm[PT_ID >= 490 & PT_ID <= 495] 
test[, anom_z := scale(anom), variable]
test_events <- test[dur > 12] 
test_cumsum <- cumsum_events(test, test_events, pre, aft, scale = T) 

test_cumsum_m <- test_cumsum[, .(median(cumsum), .N), .(variable, DTM, event)]
setnames(test_cumsum_m, 'V1', 'cumsum')
test_cumsum_m <- test_cumsum_m[complete.cases(test_cumsum_m)]
#test_cumsum_m <- test_cumsum_m[N >= grid_cell_thres]

#plotting
rects <- data.frame(DTM = test_events$start, 
                    DTM_end = test_events$start + months(test_events$dur))
rects <- unique(merge(rects, test_cumsum_m[, .(DTM, event)], by = 'DTM'))

def_vols <- mhm_dv[PT_ID == 493 & DTM %in% test_cumsum$DTM]
def_vols <- def_vols[!is.na(value)]
def_vols <- unique(merge(test_cumsum[, .(DTM, PT_ID = as.numeric(PT_ID), event)], def_vols, by = c('DTM', 'PT_ID')))

ggplot(test_cumsum_m, aes(x = DTM, y = cumsum, col = variable)) +
  geom_rect(data = rects, aes(xmin = DTM, xmax = DTM_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.3, fill = "grey70", col = "grey70", inherit.aes = F) +
  geom_bar(data = def_vols, aes(x = DTM, y = value, fill = variable), col = 'black',
           position = "dodge", stat = 'identity',  inherit.aes = F, alpha = 0.6) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) + 
  scale_fill_manual(values = var_cols[c(1, 5, 3)]) +
  scale_color_manual(values = var_cols[c(1, 5, 3, 2)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  facet_wrap(vars(event), scales = 'free_x') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts

#todo: 
ggplot(aa[variable == 's'], aes(x = factor(month(DTM)), y = anom, fill = event)) +
  geom_col(col = 'black', size = 0.75) +
  geom_hline(yintercept = 0) +
  xlab(label = 'Month') +
  ylab(label = 'Soil moisture (z-score)') + 
  scale_color_manual(values = var_cols) +
  theme_bw() +
  theme_opts

####
#working with many points

pre <- 12
aft <- 12
event_dur <- 9
my_events <- c(1921, 1976, 2003, 2017)
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


#### All grid cells

events <- unique(test[dur >= event_dur & year(start) %in% my_events, .(start, dur, PT_ID), ID])
events[, end := start + months(dur)]
events[, yr := year(start)]
events[, max_end := max(end) + months(aft), yr]
events[, min_start := min(start) - months(pre), yr]

test_anom <- rbind( #investigating the preconditions of the events
  cbind(cumsum_events_space_all(test, 'nP', events, pre, aft), var = 'nP'), 
  cbind(cumsum_events_space_all(test, 'nPET', events, pre, aft), var = 'nPET'),
  cbind(cumsum_events_space_all(test, 'nQ', events, pre, aft), var = 'nQ'),
  cbind(cumsum_events_space_all(test, 'nS', events, pre, aft), var = 'nS')) 
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




