#Properties of 2018 drought event

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_short_met1.Rds')

mhm <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p, q, s, pet)]
mhm <- melt(mhm, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
mhm[, value_z := scale(value), .(variable, PT_ID)]

mhm_dv <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p_dv, q_dv, s_dv)]
mhm_dv <- melt(mhm_dv, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
rm(dta); gc()

pre <- 12 * 10 + 1
aft <- 0
dur_event <- 3
mwin <- 50

mhm_cneu <- mhm[REG %in% c('CEU', 'NEU')]  
mhm_cneu_events <- mhm_cneu[year(start) >= 2018 & dur >= dur_event] 
min_start <- mhm_cneu_events[, min(start)]
mean_30 <- mhm_cneu[DTM < min_start - months(pre) &  #The average is estimated before preconditions
                    DTM > min_start - years(mwin) - months(pre), mean(value_z, na.rm = T), 
                  .(variable, PT_ID)]
mhm_cneu <- mhm_cneu[mean_30, on = c('variable', 'PT_ID')]
mhm_cneu[, anom := value_z - V1]

mhm_cneu_events[variable == 'p', table(dur)]
mhm_cneu_events[variable == 'p', table(start)]
mhm_cneu_events[, length(unique(PT_ID))]/mhm_cneu[, length(unique(PT_ID))] 

mhm_cneu_periods <- get_periods_par(mhm_cneu, mhm_cneu_events, pre, aft) #Period of events including pre and aft
mhm_cneu_ext <- mhm_cneu[mhm_cneu_periods, on = c('DTM', 'PT_ID')] #Drought events based on criterion + pre and aft
mhm_cneu_ext[, cumsum := cumsum(anom), c('variable', 'PT_ID')]

mhm_cneu_ext_m <- mhm_cneu_ext[, .(median(cumsum), .N), .(variable, DTM, event)]
setnames(mhm_cneu_ext_m, 'V1', 'cumsum')
mhm_cneu_ext_m <- mhm_cneu_ext_m[complete.cases(mhm_cneu_ext_m)]

#plotting
rects <- data.frame(DTM = mhm_cneu_events$start, 
                    DTM_end = mhm_cneu_events$start + months(mhm_cneu_events$dur))
rects <- unique(merge(rects, mhm_cneu_ext_m[, .(DTM, event)], by = 'DTM'))

def_vols <- mhm_dv[REG %in% c('CEU', 'NEU') & DTM %in% mhm_cneu_ext$DTM]
def_vols <- unique(merge(unique(mhm_cneu_ext[, .(DTM, PT_ID = as.numeric(PT_ID), event)]), def_vols, by = c('DTM', 'PT_ID')))
def_vols[, value := sum(value, na.rm = T), .(DTM, variable)]
def_vols[, value := value/mean(value, na.rm = T), variable]
def_vols <- unique(def_vols[, .(DTM, event, variable, value)])

ggplot(mhm_cneu_ext_m, aes(x = DTM, y = cumsum, col = variable)) +
  geom_rect(data = rects, aes(xmin = DTM, xmax = DTM_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.3, fill = "grey70", col = "grey70", inherit.aes = F) +
  geom_bar(data = def_vols, aes(x = DTM, y = value, fill = variable), col = 'black', 
           position = "dodge", stat = 'identity',  inherit.aes = F, alpha = 0.3) + 
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
