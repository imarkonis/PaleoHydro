source('./source/functions.R'); source('./source/graphics.R') 

load('./results/som/ceu/som_start_year_5_1000_3_events.Rdata')

events_som[slope == 'pos' & cluster != 22]

events_som_pos <- melt(events_som[slope == 'pos', .(ID, PT_ID, x = scale(x), y = scale(y), year, dur, start_month, 
                                       start_p3_month, start_s_month, start_q_month,  p_dv_m, s_dv_m, q_dv_m, cluster)],
                        id.vars = c('ID', 'PT_ID', 'year','cluster'))
events_som_neg <- melt(events_som[slope == 'neg', .(ID, PT_ID, x = scale(x), y = scale(y), year, dur, start_month, 
                                       start_p3_month, start_s_month, start_q_month,  p_dv_m, s_dv_m, q_dv_m, cluster)],
                        id.vars = c('ID', 'PT_ID', 'year','cluster'))

ggplot(events_som_pos, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey60', 'grey60', colset_mid[11:12], var_cols[c(1, 3, 4)], var_cols[c(1, 3, 4)])) +
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3)) +
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))

ggplot(events_som_neg, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey60', 'grey60', colset_mid[11:12], var_cols[c(1, 3, 4)], var_cols[c(1, 3, 4)])) +
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3)) +
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))

events_som_1 <- events_som[year < mean(year)]
events_som_2 <- events_som[year > mean(year)]


#End of winter droughts (veg period ?)
events_som_1[start_month %in% 2:3, .(.N, dur = median(dur))]
events_som_2[start_month %in% 2:3, .(.N, dur = median(dur))]

aa <- events_som_2[start_month %in% 2:3, .N, year]
setorder(aa, year)

#Winter runoff droughts 
events_som_1[start_month %in% c(1, 12), .(.N, dur = median(dur))]
events_som_2[start_month %in% c(1, 12), .(.N, dur = median(dur))]

events_som_1[start_month %in% c(1, 12) & start_s_month == 0, .(.N, dur = median(dur))]
events_som_2[start_month %in% c(1, 12) & start_s_month == 0, .(.N, dur = median(dur))]

#Autumn runoff droughts -> combined droughts
events_som_1[start_month %in% 10, .(.N, dur = median(dur))]
events_som_2[start_month %in% 10, .(.N, dur = median(dur))]

aa <- events_som_1[start_month %in% 10 & start_s_month == 0, .N, year]
setorder(aa, year)

events_som_1[start_month %in% 10 & start_s_month == 0, .(.N, dur = median(dur))]
events_som_2[start_month %in% 10 & start_s_month == 0, .(.N, dur = median(dur))]

events_som_1[start_month %in% 10 & dur <= 6 & start_s_month > 0, .(.N, dur = median(dur))]
events_som_2[start_month %in% 10 & dur <= 6 & start_s_month > 0, .(.N, dur = median(dur))]

#Summer droughts
events_som_1[start_month %in% 4:6, .(.N, dur = median(dur))]
events_som_2[start_month %in% 4:6, .(.N, dur = median(dur))]

events_som_1[start_month %in% 4:6 & dur >= 4, .(.N, dur = median(dur))]
events_som_2[start_month %in% 4:6 & dur >= 4, .(.N, dur = median(dur))]

