source('./source/functions.R'); source('./source/graphics.R') 

dta <- readRDS('./data/mstat_short_met1.Rds')
dta_sp <- readRDS('./data/spatial.Rds')
cz_sp <- dta_sp[LAT >= 48 & LAT <= 50 & LON >= 13 & LON <= 17, PT_ID]
sca_sp <- dta_sp[LAT >= 55 & LAT <= 65 & LON >= 5 & LON <= 30, PT_ID]
ger_sp <- dta_sp[LAT >= 47 & LAT <= 55 & LON >= 5 & LON <= 15, PT_ID]

mhm <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p, q, s, pet)]
mhm <- melt(mhm, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
mhm[, value_z := scale(value), .(variable, PT_ID)]

mhm_dv <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p_dv, q_dv, s_dv)]
mhm_dv <- melt(mhm_dv, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
rm(dta); gc()

event_1921 <- mhm_dv[year(DTM) == 1921 & PT_ID %in% cz_sp]
event_1921_prpg <- space_prpg(event_1921)
event_2018 <- mhm_dv[year(DTM) == 2018 & PT_ID %in% cz_sp]
event_2018_prpg <- space_prpg(event_2018)
event_2018_dur <- month(event_2018_prpg$end[2]) - month(event_2018_prpg$start[2]) - 1

event_1921_month <- mhm_dv[DTM >= event_1921_prpg$start[2] &
                             DTM <= event_1921_prpg$start[2] + months(event_2018_dur) &
                             PT_ID %in% unique(event_1921[DTM == event_1921_prpg$peak[2] & 
                                                            variable == 'q_dv' & 
                                                            !is.na(value), PT_ID]), 
                           sum(value, na.rm = T), 
                           .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_1921_month, 'V1', 'sum')
event_1921_month[, rel_month := month - month(event_1921_prpg$start[2]) + 1]

event_2018_month <- event_2018[DTM >= event_2018_prpg$start[2] & 
                                 PT_ID %in% unique(event_2018[DTM == event_2018_prpg$peak[2] & 
                                                                variable == 'q_dv' & 
                                                                !is.na(value), PT_ID]), 
                               sum(value, na.rm = T), 
                               .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2018_month, 'V1', 'sum')
event_2018_month[, rel_month := month - month(event_2018_prpg$start[2]) + 1]

event_comp <- rbind(event_1921_month, event_2018_month)
event_comp[, cumsum := cumsum(sum), .(variable, year)]

ggplot(event_comp, aes(x = rel_month, y = cumsum, col = year)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(2, 5)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  facet_wrap(vars(variable), scales = 'free_y') +
  ggtitle('Runoff drought in Czech Republic', subtitle = paste('1921 drought starting month:', month(event_1921_prpg$start[2]))) + 
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('./results/figs/event_comp_cz.png', height = 3)

#SCANDINAVIA 
event_1921 <- mhm_dv[year(DTM) == 1921 & PT_ID %in% sca_sp]
event_1921_prpg <- space_prpg(event_1921)
event_2018 <- mhm_dv[year(DTM) == 2018 & PT_ID %in% sca_sp]
event_2018_prpg <- space_prpg(event_2018)
event_2018_dur <- month(event_2018_prpg$end[2]) - month(event_2018_prpg$start[2]) - 1

event_1921_month <- mhm_dv[DTM >= as.Date('1921-02-01') & 
                               DTM <= as.Date('1921-02-01') + months(event_2018_dur) &
                               PT_ID %in% unique(event_1921[DTM == event_1921_prpg$peak[2] & 
                                                              variable == 'q_dv' & 
                                                              !is.na(value), PT_ID]), 
                               sum(value, na.rm = T), 
                               .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_1921_month, 'V1', 'sum')
event_1921_month[, rel_month := month - 2 + 1]

event_2018_month <- event_2018[DTM >= event_2018_prpg$start[2] & 
                                PT_ID %in% unique(event_2018[DTM == event_2018_prpg$peak[2] & 
                                                                variable == 'q_dv' & 
                                                               !is.na(value), PT_ID]), 
                                sum(value, na.rm = T), 
                                .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2018_month, 'V1', 'sum')
event_2018_month[, rel_month := month - month(event_2018_prpg$start[2]) + 1]

event_comp <- rbind(event_1921_month, event_2018_month)
event_comp[, cumsum := cumsum(sum), .(variable, year)]

ggplot(event_comp, aes(x = rel_month, y = cumsum, col = year)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(2, 5)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  facet_wrap(vars(variable), scales = 'free_y') +
  ggtitle('Runoff drought in Scandinavia', subtitle = paste('1921 drought starting month:', month(event_1921_prpg$start[2]))) + 
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('./results/figs/event_comp_sca.png', height = 3)

#GERMANY
event_1921 <- mhm_dv[year(DTM) == 1921 & PT_ID %in% ger_sp]
event_1921_prpg <- space_prpg(event_1921)
event_2018 <- mhm_dv[year(DTM) == 2018 & PT_ID %in% ger_sp]
event_2018_prpg <- space_prpg(event_2018)
event_2018_dur <- month(event_2018_prpg$end[2]) - month(event_2018_prpg$start[2]) - 1

event_1921_month <- mhm_dv[DTM >= as.Date('1921-02-01') & 
                             DTM <= as.Date('1921-02-01') + months(event_2018_dur) &
                             PT_ID %in% unique(event_1921[DTM == event_1921_prpg$peak[2] & 
                                                            variable == 'q_dv' & 
                                                            !is.na(value), PT_ID]), 
                           sum(value, na.rm = T), 
                           .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_1921_month, 'V1', 'sum')
event_1921_month[, rel_month := month - 2 + 1]

event_2018_month <- event_2018[DTM >= event_2018_prpg$start[2] & 
                                 PT_ID %in% unique(event_2018[DTM == event_2018_prpg$peak[2] & 
                                                                variable == 'q_dv' & 
                                                                !is.na(value), PT_ID]), 
                               sum(value, na.rm = T), 
                               .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2018_month, 'V1', 'sum')
event_2018_month[, rel_month := month - 3 + 1]

event_comp <- rbind(event_1921_month, event_2018_month)
event_comp[, cumsum := cumsum(sum), .(variable, year)]

ggplot(event_comp, aes(x = rel_month, y = cumsum, col = year)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(2, 5)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  facet_wrap(vars(variable), scales = 'free_y') +
  ggtitle('Runoff drought in Germany', subtitle = paste('1921 drought starting month: 2')) + 
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('./results/figs/event_comp_ger.png', height = 3)

#COMPOSITE CEU [2018]
event_2018 <- mhm_dv[year(DTM) == 2018 & REG == 'CEU']
event_2018_prpg <- space_prpg(event_2018)
event_2018_dur <- month(event_2018_prpg$end[2]) - month(event_2018_prpg$start[2]) - 1
event_1921 <- mhm_dv[year(DTM) == 1921 &  PT_ID %in% unique(event_2018$PT_ID)]
event_1921_prpg <- space_prpg(event_1921)

event_1921_month <- mhm_dv[DTM >= as.Date('1921-02-01') & 
                             DTM <= as.Date('1921-02-01') + months(event_2018_dur) &
                             PT_ID %in% unique(event_1921[DTM == event_1921_prpg$peak[2] & 
                                                            variable == 'q_dv' & 
                                                            !is.na(value), PT_ID]), 
                           sum(value, na.rm = T), 
                           .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_1921_month, 'V1', 'sum')
event_1921_month[, rel_month := month - 2 + 1]

event_2018_month <- event_2018[DTM >= event_2018_prpg$start[2] & 
                                 PT_ID %in% unique(event_2018[DTM == event_2018_prpg$peak[2] & 
                                                                variable == 'q_dv' & 
                                                                !is.na(value), PT_ID]), 
                               sum(value, na.rm = T), 
                               .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2018_month, 'V1', 'sum')
event_2018_month[, rel_month := month - month(event_2018_prpg$start[2]) + 1]

event_comp <- rbind(event_1921_month, event_2018_month)
setorder(event_comp, variable, year, rel_month)
event_comp[, cumsum := cumsum(sum), .(variable, year)]

ggplot(event_comp, aes(x = rel_month, y = cumsum, col = year)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(2, 5)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  facet_wrap(vars(variable), scales = 'free_y') +
  ggtitle('Runoff drought in CEU (cells with drought in 2018)', subtitle = paste('1921 drought starting month: 2')) + 
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('./results/figs/event_comp_ceu_2018.png', height = 3)

