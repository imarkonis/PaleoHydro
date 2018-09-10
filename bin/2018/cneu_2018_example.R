#Properties of 2018 drought event

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_short_met1.Rds')

mhm <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p, q, s, pet)]
mhm <- melt(mhm, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
mhm[, anom := value - mean(value, na.rm = T), variable]

mhm_dv <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p_dv, q_dv, s_dv)]
mhm_dv <- melt(mhm_dv, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
rm(dta); gc()

pre <- 12 
aft <- 0
dur_event <- 3

mhm_cneu <- mhm[REG %in% c('CEU', 'NEU')] 
mhm_cneu[, anom_z := scale(anom), variable]
mhm_cneu_events <- mhm_cneu[start >= '2017-09-01' & dur >= dur_event] 

mhm_cneu_events[variable == 'p', table(dur)]
mhm_cneu_events[variable == 'p', table(start)]
mhm_cneu_events[, length(unique(PT_ID))]/mhm_cneu[, length(unique(PT_ID))] 

mhm_cneu_events[variable == 'p' & start <= '2017-11-01', table(dur)]
mhm_cneu_events_remove <- mhm_cneu_events[start <= '2018-1-01' & dur <= 8, unique(ID)] 
mhm_cneu_events <- mhm_cneu_events[!(ID %in% mhm_cneu_events_remove)]

mhm_cneu_cumsum <- cumsum_events(mhm_cneu, mhm_cneu_events, pre, aft, scale = T, par = T) 
mhm_cneu_cumsum_m <- mhm_cneu_cumsum[, .(median(cumsum), .N), .(variable, DTM, event)]
setnames(mhm_cneu_cumsum_m, 'V1', 'cumsum')
mhm_cneu_cumsum_m <- mhm_cneu_cumsum_m[complete.cases(mhm_cneu_cumsum_m)]
mhm_cneu_cumsum_m <- mhm_cneu_cumsum_m[N >= grid_cell_thres]

#plotting
rects <- data.frame(DTM = mhm_cneu_events$start, 
                    DTM_end = mhm_cneu_events$start + months(mhm_cneu_events$dur))
rects <- unique(merge(rects, mhm_cneu_cumsum_m[, .(DTM, event)], by = 'DTM'))

def_vols <- mhm_dv[REG %in% c('CEU', 'NEU') & DTM %in% mhm_cneu_cumsum$DTM]
def_vols <- def_vols[!is.na(value)]
def_vols <- unique(merge(mhm_cneu_cumsum[, .(DTM, PT_ID = as.numeric(PT_ID), event)], def_vols, by = c('DTM', 'PT_ID')))

ggplot(mhm_cneu_cumsum_m, aes(x = DTM, y = cumsum, col = variable)) +
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
