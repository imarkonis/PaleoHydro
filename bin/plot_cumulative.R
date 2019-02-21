#Explore the preconditions of drought events

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_short_met1.Rds')
load('./results/som/ceu/som_start_end_5_10000_3_events.Rdata')

mhm <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p, q, s, pet)]
mhm <- melt(mhm, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))

mhm_dv <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p_dv, q_dv, s_dv)]
mhm_dv <- melt(mhm_dv, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
rm(dta); gc()

pre <- 0
aft <- 12
dur_event <- 12
grid_cell_ID <- 1698

test <- mhm[PT_ID %in% grid_cell_ID] 
test_events <- test[dur > dur_event] #1st example
#test_events <- test[year(start) == 1920 & dur > 3] #2nd example
#test_events <- test[year(start) == 2000] #3d example

event_dur <- unique(test_events[, .(start, end = start + months(dur))])
event_dur[, event := as.factor(year(start))]
event_dur[year(start) != year(end), event := paste0(year(start), '-', year(end))]
event_dur[, start := start - months(pre)]
event_dur[, end := end + months(aft)]
event_dur <- event_dur[complete.cases(event_dur)]

event_dur_seq <- foreach(i = 1:nrow(event_dur), .combine = 'rbind') %do% {
  data.table(DTM = seq(event_dur$start[i], event_dur$end[i], 'month'), event = event_dur$event[i])
}

test_events <- foreach(i = 1:nrow(event_dur), .combine = 'rbind') %do% {
  local_scale(test, test[DTM >= event_dur$start[i] & DTM <= event_dur$end[i]])
}

test_events_cumsum <- foreach(i = 1:nrow(event_dur), .combine = 'rbind') %do% {
  test_events[DTM >= event_dur$start[i] & DTM <= event_dur$end[i], .(DTM, cumsum = cumsum(value_anom_30)), variable]
}

test_events_cumsum <- merge(test_events_cumsum, event_dur_seq, by='DTM', allow.cartesian = T)

#ploting
rects <- data.frame(DTM = test_events$start, 
                    DTM_end = test_events$start + months(test_events$dur))
rects <- unique(merge(rects, test_events_cumsum[, .(DTM, event)], by = 'DTM'))

def_vols <- mhm_dv[PT_ID == grid_cell_ID & DTM %in% test_cumsum$DTM]
def_vols <- def_vols[!is.na(value)]
def_vols <- unique(merge(unique(test_cumsum[, .(DTM, PT_ID = as.numeric(PT_ID), event)]), 
                         def_vols, by = c('DTM', 'PT_ID')))

ggplot(test_events_cumsum, aes(x = DTM, y = cumsum, col = variable)) +
  geom_rect(data = rects, aes(xmin = DTM, xmax = DTM_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.3, fill = "grey70", col = "grey70", inherit.aes = F) +
 # geom_bar(data = def_vols, aes(x = DTM, y = value, fill = variable), col = 'black',
  #         position = "dodge", stat = 'identity',  inherit.aes = F, alpha = 0.6) + 
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
grid_cell_thres = 30 #min number of grid cells to estimate median

mhm_ceu <- mhm[REG == 'CEU'] 
mhm_ceu[, anom_z := scale(anom), variable]
mhm_ceu_events <- mhm_ceu[dur >= dur_event] 
mhm_ceu_cumsum <- cumsum_events(mhm_ceu, mhm_ceu_events, pre, aft, scale = T, par = T) 

mhm_ceu_cumsum_m <- mhm_ceu_cumsum[, .(median(cumsum), .N), .(variable, DTM, event)]
setnames(mhm_ceu_cumsum_m, 'V1', 'cumsum')
mhm_ceu_cumsum_m <- mhm_ceu_cumsum_m[complete.cases(mhm_ceu_cumsum_m)]
mhm_ceu_cumsum_m <- mhm_ceu_cumsum_m[N >= grid_cell_thres]

#plotting
rects <- data.frame(DTM = mhm_ceu_events$start, 
                    DTM_end = mhm_ceu_events$start + months(mhm_ceu_events$dur))
rects <- unique(merge(rects, mhm_ceu_cumsum_m[, .(DTM, event)], by = 'DTM'))

def_vols <- mhm_dv[PT_ID == 493 & DTM %in% mhm_ceu_cumsum$DTM]
def_vols <- def_vols[!is.na(value)]
def_vols <- unique(merge(mhm_ceu_cumsum[, .(DTM, PT_ID = as.numeric(PT_ID), event)], def_vols, by = c('DTM', 'PT_ID')))

ggplot(mhm_ceu_cumsum_m, aes(x = DTM, y = cumsum, col = variable)) +
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
