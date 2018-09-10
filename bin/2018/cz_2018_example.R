#Properties of 2018 drought event

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_short_met1.Rds')
dta_sp <- readRDS('./data/spatial.Rds')
cz_sp <- dta_sp[LAT >= 48 & LAT <= 52 & LON >= 13 & LON <= 17, PT_ID]

mhm <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p, q, s, pet)]
mhm <- melt(mhm, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
mhm[, anom := value - mean(value, na.rm = T), variable]

mhm_dv <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p_dv, q_dv, s_dv)]
mhm_dv <- melt(mhm_dv, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
rm(dta); gc()

pre <- 12 * 10
aft <- 0
dur_event <- 3

mhm_cz <- mhm[PT_ID %in% cz_sp] 
mhm_cz[, anom_z := scale(anom), variable]
mhm_cz_events <- mhm_cz[year(start) >= 2018 & dur >= dur_event] 

mhm_cz_events[variable == 'p', table(dur)]
mhm_cz_events[variable == 'p', table(start)]
mhm_cz_events[, length(unique(PT_ID))]/mhm_cz[, length(unique(PT_ID))] 

mhm_cz_cumsum <- cumsum_events(mhm_cz, mhm_cz_events, pre, aft, scale = T, par = F) 
mhm_cz_cumsum_m <- mhm_cz_cumsum[, .(median(cumsum), .N), .(variable, DTM, event)]
setnames(mhm_cz_cumsum_m, 'V1', 'cumsum')
mhm_cz_cumsum_m <- mhm_cz_cumsum_m[complete.cases(mhm_cz_cumsum_m)]
mhm_cz_cumsum_m <- mhm_cz_cumsum_m[N >= grid_cell_thres]

#plotting
rects <- data.frame(DTM = mhm_cz_events$start, 
                    DTM_end = mhm_cz_events$start + months(mhm_cz_events$dur))
rects <- unique(merge(rects, mhm_cz_cumsum_m[, .(DTM, event)], by = 'DTM'))

def_vols <- mhm_dv[PT_ID %in% cz_sp & DTM %in% mhm_cz_cumsum$DTM]
def_vols <- def_vols[!is.na(value)]
def_vols <- unique(merge(mhm_cz_cumsum[, .(DTM, PT_ID = as.numeric(PT_ID), event)], def_vols, by = c('DTM', 'PT_ID')))

ggplot(mhm_cz_cumsum_m, aes(x = DTM, y = cumsum, col = variable)) +
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
