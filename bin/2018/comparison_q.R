source('./source/functions.R'); source('./source/graphics.R') 

dta <- readRDS('./data/mstat_short_met1.Rds')
dta_sp <- readRDS('./data/spatial.Rds')
cz_sp <- dta_sp[LAT >= 48 & LAT <= 50 & LON >= 13 & LON <= 17, PT_ID]
sca_sp <- dta_sp[LAT >= 55 & LAT <= 65 & LON >= 5 & LON <= 30, PT_ID]
ger_sp <- dta_sp[LAT >= 47 & LAT <= 55 & LON >= 5 & LON <= 15, PT_ID]

mhm_dv <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p3_dv, q_dv, s_dv)]
mhm_dv <- melt(mhm_dv, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))

#CEU & NEU sum
event_2018 <- mhm_dv[year(DTM) == 2018 & (REG == 'CEU' | REG == 'NEU')]
event_2018_prpg <- space_prpg(event_2018)
event_2018_dur <- month(event_2018_prpg$end[2]) - month(event_2018_prpg$start[2]) - 1
event_1921 <- mhm_dv[year(DTM) == 1921 & (REG == 'CEU' | REG == 'NEU')]
event_1921_prpg <- space_prpg(event_1921, same_year = T)
event_2003 <- mhm_dv[year(DTM) == 2003 & (REG == 'CEU' | REG == 'NEU')]
event_2003_prpg <- space_prpg(event_2003, same_year = T)
event_2015 <- mhm_dv[year(DTM) == 2015 & (REG == 'CEU' | REG == 'NEU')]
event_2015_prpg <- space_prpg(event_2015)

event_1921_month <- mhm_dv[DTM >= event_1921_prpg$start[2] &
                             DTM <= event_1921_prpg$start[2] + months(event_2018_dur) &
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

event_2003_month <- mhm_dv[DTM >= event_2003_prpg$start[2] &
                             DTM <= event_2003_prpg$start[2] + months(event_2018_dur) &
                             PT_ID %in% unique(event_2003[DTM == event_2003_prpg$peak[2] & 
                                                            variable == 'q_dv' & 
                                                            !is.na(value), PT_ID]), 
                           sum(value, na.rm = T), 
                           .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2003_month, 'V1', 'sum')
event_2003_month[, rel_month := month - 2 + 1]

event_2015_month <- event_2015[DTM >= event_2015_prpg$start[2] & 
                                 DTM <= event_2015_prpg$start[2] + months(event_2018_dur) &
                                 PT_ID %in% unique(event_2015[DTM == event_2015_prpg$peak[2] & 
                                                                variable == 'q_dv' & 
                                                                !is.na(value), PT_ID]), 
                               sum(value, na.rm = T), 
                               .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2015_month, 'V1', 'sum')
event_2015_month[, rel_month := month - month(event_2015_prpg$start[2]) + 1]

event_comp <- rbind(event_1921_month, event_2018_month, event_2003_month, event_2015_month)
setorder(event_comp, variable, year, rel_month)
event_comp[, cumsum := cumsum(sum), .(variable, year)]

ggplot(event_comp, aes(x = rel_month, y = cumsum, col = year)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(2, 1, 4, 5)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum') + 
  facet_wrap(vars(variable), scales = 'free_y') +
  ggtitle('Runoff drought in CEU & NEU (only cells with drought)') + 
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('./results/figs/2018/compare_cneu_q_sum.png', height = 3)

#CEU & NEU mean
event_1921_month <- mhm_dv[DTM >= event_1921_prpg$start[2] &
                             DTM <= event_1921_prpg$start[2] + months(event_2018_dur) &
                             PT_ID %in% unique(event_1921[DTM == event_1921_prpg$peak[2] & 
                                                            variable == 'q_dv' & 
                                                            !is.na(value), PT_ID]), 
                           mean(value, na.rm = T), 
                           .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_1921_month, 'V1', 'mean')
event_1921_month[, rel_month := month - 2 + 1]

event_2018_month <- event_2018[DTM >= event_2018_prpg$start[2] & 
                                 PT_ID %in% unique(event_2018[DTM == event_2018_prpg$peak[2] & 
                                                                variable == 'q_dv' & 
                                                                !is.na(value), PT_ID]), 
                               mean(value, na.rm = T), 
                               .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2018_month, 'V1', 'mean')
event_2018_month[, rel_month := month - month(event_2018_prpg$start[2]) + 1]

event_2003_month <- mhm_dv[DTM >= event_2003_prpg$start[2] &
                             DTM <= event_2003_prpg$start[2] + months(event_2018_dur) &
                             PT_ID %in% unique(event_2003[DTM == event_2003_prpg$peak[2] & 
                                                            variable == 'q_dv' & 
                                                            !is.na(value), PT_ID]), 
                           mean(value, na.rm = T), 
                           .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2003_month, 'V1', 'mean')
event_2003_month[, rel_month := month - 2 + 1]

event_2015_month <- event_2015[DTM >= event_2015_prpg$start[2] & 
                                 DTM <= event_2015_prpg$start[2] + months(event_2018_dur) &
                                 PT_ID %in% unique(event_2015[DTM == event_2015_prpg$peak[2] & 
                                                                variable == 'q_dv' & 
                                                                !is.na(value), PT_ID]), 
                               mean(value, na.rm = T), 
                               .(variable, month(DTM), year = factor(year(DTM)))]
setnames(event_2015_month, 'V1', 'mean')
event_2015_month[, rel_month := month - month(event_2015_prpg$start[2]) + 1]

event_comp <- rbind(event_1921_month, event_2018_month, event_2003_month, event_2015_month)
setorder(event_comp, variable, year, rel_month)
event_comp[, cumsum := cumsum(mean), .(variable, year)]

ggplot(event_comp, aes(x = rel_month, y = cumsum, col = year)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(2, 1, 4, 5)]) +
  xlab(label = "Time (months)") +
  ylab(label = 'Cumulative sum of monthly mean') + 
  facet_wrap(vars(variable), scales = 'free_y') +
  ggtitle('Runoff drought in CEU & NEU (only cells with drought)') + 
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('./results/figs/2018/compare_cneu_q_mean.png', height = 3)



