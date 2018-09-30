source('./source/functions.R'); source('./source/graphics.R') 

load('./results/som/ceu/som_start_end_5_10000_3_events.Rdata')

cl_ids <- events_som[cluster %in% c(cl_inc, cl_dec), unique(cluster)]
events_chng <- events_som[cluster %in% cl_ids]

cl_23 <- events_som[cluster == 23]


to_plot = rbind(data.table(melt(cl_23[, .(p = p_dv_m, s = s_dv_m, q = q_dv_m)]), cl = factor(23)), 
                data.table(melt(cl_20[, .(p = p_dv_m, s = s_dv_m, q = q_dv_m)]), cl = factor(20)))

to_plot <- to_plot[value > 0]
ggplot(to_plot, aes(x = value, col = cl)) +
  geom_density() +
  facet_free(~variable) +
  theme_bw()

to_plot <- melt(events_chng[, .(p = p_dv_m, s = s_dv_m, q = q_dv_m, slope, cluster, pet = pet_ev_m)], id.vars = c('slope', 'cluster')) 
to_plot <- to_plot[value > 0]
to_plot$slope <- factor(to_plot$slope)
to_plot$cluster<- factor(to_plot$cluster)
ggplot(to_plot, aes(x = value, col = slope)) +
  scale_color_manual(values = c('black', 'red')) +
  scale_x_continuous(limits = c(0, 5)) +
  geom_density() +
  facet_free(~variable) +
  theme_bw()








gg_dur <- ggplot(events_som[n_clusters > 50], aes(x = factor(cluster), fill = factor(dur_cat))) +
  geom_bar(position = 'stack') +
  geom_hline(yintercept = 1500, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 1000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 500, linetype = 2, col = 'grey40') +
  scale_fill_manual(values = period_cols) +
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(title = "Duration")) +
  labs(x = 'Cluster', y = 'Size') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 
ggsave(paste0(fig_path, fname, '_dur_cat_cls.png'), plot = gg_dur, height = 8, width = 14)



#Cross-validation
p3_matrix_start <- events_som[period == '1900-1939' & dur <= 12, table(start_month, dur)]
p2_matrix_start <- events_som[period == '1940-1978' & dur <= 12, table(start_month, dur)]
p1_matrix_start <- events_som[period == '1979-2018' & dur <= 12, table(start_month, dur)]
dur_start_chng <- p3_matrix_start/p2_matrix_start

ggplot(melt(dur_start_chng), aes(factor(start_month), factor(dur))) + 
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = 'dark blue',
                       mid = 'grey80',
                       high = palette_spectral(100)[70],
                       midpoint = 1) +
  geom_text(aes(label = round(value, 2))) +
  theme_bw()


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

