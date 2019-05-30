source('./source/functions.R'); source('./source/graphics.R') 
library(dplyr)

load('./results/som/ceu/som_start_end_5_10000_3_events.Rdata')

###Cluster composition fraction per year
cluster_comp <- events_som[, .(year, ID, cluster)]
cluster_comp[, sum_cls_yr := .N, year]
cluster_comp <- unique(cluster_comp[, .(.N, .N/sum_cls_yr), .(cluster, year)])
setnames(cluster_comp, 'V2', 'fraction')
setorder(cluster_comp, year, -N)
cluster_comp[, fraction := cumsum(fraction), year]
cluster_comp[, n_rank := rank(-N, ties.method = "first"), year]
ggplot(cluster_comp, aes(factor(n_rank), y = fraction, group = n_rank)) +
  geom_boxplot(fill = period_cols[c(rep(3, 3), rep(2, 3), rep(1, 18))]) +
  coord_cartesian(xlim = c(0, 10)) +
  labs(x = 'Rank', y = 'Cumulative fraction') +
  theme_bw()
ggsave("./results/figs/som/composition_cum.png", width = 5, height = 3)

cluster_comp_sum <- cluster_comp
cluster_comp_sum[n_rank <= 3, top_2_sum := max(fraction), year]
cluster_comp_sum[n_rank > 3 & n_rank <= 6, mid_24_sum := max(fraction, na.rm = T), year]
to_plot <-  merge(unique(cluster_comp_sum[, .(year, top_2_sum)]), unique(cluster_comp_sum[, .(year, mid_24_sum)]), by = 'year')
to_plot <- to_plot[complete.cases(to_plot)]
to_plot$bot_4_sum <- 1 - to_plot$mid_24_sum
to_plot[, mid_24_sum := mid_24_sum - top_2_sum]
to_plot <- to_plot[complete.cases(to_plot)]
to_plot <- melt(to_plot, id.vars = 'year')
to_plot[, variable := factor(variable, levels = c('bot_4_sum', 'mid_24_sum', 'top_2_sum'), 
                             labels = c('ranks 7-25', 'ranks 4-6', 'ranks 1-3'))]

ggplot(to_plot, aes(year, y = value, fill = variable)) +
  geom_bar(position = 'stack', stat = 'identity', col = 'black') +
  theme_bw() +
  labs(x = 'Year', y = 'cumulative fraction') +
  scale_fill_manual(values = period_cols)
ggsave("./results/figs/som/composition_year.png", width = 8, height = 4)

#Start-End-Duration
to_plot <- events_som[, .(cluster, Start = start_month, End = end_month, Duration = dur)]
to_plot <- melt(to_plot, id.vars = c("cluster"))

ggplot(to_plot[value <= 12], aes(x = factor(cluster), fill = factor(value))) +
  geom_bar(position = 'stack') +
  geom_hline(yintercept = 4000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 3000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 2000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 1000, linetype = 2, col = 'grey40') +
  scale_fill_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  coord_flip() +
  theme_bw() +
  guides(fill =guide_legend(title = "Month/Duration", ncol = 2)) +
  labs(x = 'Node', y = 'Number of events') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~variable, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 
ggsave("./results/figs/ceu_cls.png", plot = gg_start, height = 8, width = 14)



#Start
gg_start <- ggplot(events_som[n_clusters > 500], aes(x = factor(cluster), fill = factor(start_month))) +
  geom_bar(position = 'stack') +
  geom_hline(yintercept = 1500, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 1000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 500, linetype = 2, col = 'grey40') +
  scale_fill_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  coord_flip() +
  theme_bw() +
  guides(fill =guide_legend(title = "Month", ncol = 2)) +
  labs(x = 'Node', y = 'Number of events') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 
ggsave("./results/figs/ceu_start_month_cls.png", plot = gg_start, height = 8, width = 14)

#End
gg_end <- ggplot(events_som[n_clusters > 500], aes(x = factor(cluster), fill = factor(end_month))) +
  geom_bar(position = 'stack') +
  geom_hline(yintercept = 1500, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 1000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 500, linetype = 2, col = 'grey40') +
  scale_fill_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  coord_flip() +
  theme_bw() +
  guides(fill=guide_legend(title = "Month", ncol = 2)) +
  labs(x = 'Node', y = 'Number of events') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 
ggsave('./results/figs/ceu_end_month_cls.png', plot = gg_end, height = 8, width = 14)

###Duration of annual droughts
gg_dur_12 <- ggplot(events_som[n_clusters > 500 & dur <= 12], aes(x = factor(cluster), fill = factor(dur))) +
  geom_bar(position = 'stack') +
  geom_hline(yintercept = 1500, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 1000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 500, linetype = 2, col = 'grey40') +
  scale_fill_manual(values = palette_mid(18)[9:18]) +
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(title = "Duration", ncol = 2)) +
  labs(x = 'Node', y = 'Number of events') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 
ggsave(paste0('./results/figs/ceu_dur_12_cls.png'), plot = gg_dur_12, height = 8, width = 14)

###Duration Categories
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

gg_all <- ggarrange(gg_start, gg_end, 
                    gg_dur,  gg_dur_12, 
                    labels = c("a", "b", 'c', 'd'),
                    nrow = 4, ncol = 1)

ggsave(paste0(fig_path, fname, '_cls_sum.png'), plot =gg_all, height = 20, width = 18)

####################################################################
#Clusters with most significant change
####################################################################

#trends
gg_slopes <- ggplot(n_cls_yr[cluster %in% c(cl_inc, cl_dec)], aes(x = year, y = N)) +
  geom_bar(stat = 'identity') +
  geom_smooth(se = F, method = "lm", col = colset_mid[11]) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_fill_manual(values = palette_mid(5)) +
  scale_linetype_manual(values = rep(1:5, 5)) +  
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() + 
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 
ggsave(paste0(fig_path, fname, '_cls_trends.png'), height = 8, width = 14)

#Start
gg_start <- ggplot(events_som[cluster %in% c(cl_inc, cl_dec)], aes(x = factor(cluster), fill = factor(start_month))) +
  geom_bar(position = 'stack') +
  geom_hline(yintercept = 1500, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 1000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 500, linetype = 2, col = 'grey40') +
  scale_fill_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  coord_flip() +
  theme_bw() +
  guides(fill =guide_legend(title = "Month", ncol = 2)) +
  labs(x = 'Cluster', y = 'Size') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 

#End
gg_end <- ggplot(events_som[cluster %in% c(cl_inc, cl_dec)], aes(x = factor(cluster), fill = factor(end_month))) +
  geom_bar(position = 'stack') +
  geom_hline(yintercept = 1500, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 1000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 500, linetype = 2, col = 'grey40') +
  scale_fill_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  coord_flip() +
  theme_bw() +
  guides(fill=guide_legend(title = "Month", ncol = 2)) +
  labs(x = 'Cluster', y = 'Size') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 

###Duration of annual droughts
gg_dur_12 <- ggplot(events_som[cluster %in% c(cl_inc, cl_dec) & dur <= 12], aes(x = factor(cluster), fill = factor(dur))) +
  geom_bar(position = 'stack') +
  geom_hline(yintercept = 1500, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 1000, linetype = 2, col = 'grey40') +
  geom_hline(yintercept = 500, linetype = 2, col = 'grey40') +
  scale_fill_manual(values = palette_mid(36)) +
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(title = "Duration", ncol = 2)) +
  labs(x = 'Cluster', y = 'Size') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 

###Drought type composition
to_plot <- melt(events_som[cluster %in% c(cl_inc, cl_dec), 
                           .(cluster, start_p3_month, start_q_month, start_s_month)],  id.vars = c('cluster'))
to_plot[variable == 'start_p3_month' & value > 0, drought := factor('Met.')]
to_plot[variable == 'start_q_month' & value > 0, drought := factor('Hyd.')]
to_plot[variable == 'start_s_month' & value > 0, drought := factor('Agr.')]
to_plot[variable == 'start_p3_month' & value == 0, drought := factor('no_p')]
to_plot[variable == 'start_q_month' & value == 0, drought := factor('no_q')]
to_plot[variable == 'start_s_month' & value == 0, drought := factor('no_s')]
to_plot <- to_plot %>% group_by(cluster, variable, drought) %>% tally()

gg_type <- ggplot(data = to_plot, aes(x = variable, y = n, fill = factor(drought))) +
  geom_bar(stat = "identity", position = position_fill(), width = 1, color = 'grey20') +
  facet_wrap(~cluster) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(var_cols[1:3], var_cols_alpha), breaks = c("Met.", "Hyd.", "Agr.")) +
  theme_void() +
  theme(aspect.ratio = 1) +
  guides(fill = guide_legend(title = "Drought Type"))

###Drought intensity (deficit volumes)
to_plot <- melt(events_som[cluster %in% c(cl_inc, cl_dec),
                           .(p = p_dv_m, q = q_dv_m, slope, s = s_dv_m, cluster)], id.vars = c('slope', 'cluster')) 
to_plot <- to_plot[value > 0]
to_plot$slope <- factor(to_plot$slope)
to_plot$cluster<- factor(to_plot$cluster)

gg_dv <- ggplot(to_plot, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.colour = NA) + 
  scale_y_continuous(limits = c(0, 3)) +
  geom_hline(yintercept = 1, linetype = 2, col = 'grey30') +
  facet_wrap(~cluster) +
  scale_fill_manual(values = c(var_cols[1:3])) +
  theme_bw() +
  theme_opts +
  labs(x = 'Variable', y = 'Mean def. volume') +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80'))  

gg_last  <- ggarrange(gg_slopes, gg_type, gg_dv,  
                      labels = c("a", "b", 'c'),
                      nrow = 1, ncol = 3)
ggsave(filename = paste0(fig_path, fname, '_cls_sum2_sig.png'), plot = gg_last, height = 4, width = 18)

gg_season <- ggarrange(gg_start, gg_end, gg_dur_12,
                       labels = c("a", "b", 'c'),
                       nrow = 3, ncol = 1)
ggsave(paste0(fig_path, fname, '_cls_sum1_sig.png'), plot = gg_season, height = 8, width = 8)


##Other plots
#clusters in space (map)
ggplot(events_som, aes(x, y, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  facet_wrap(~cluster) +
  theme_bw()
ggsave(paste0(fig_path, fname, '_map.png'), height = 8, width = 14)

#cluster properties (boxplot)
ggplot(events_som_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey60', 'grey60', colset_mid[11:12], var_cols[c(1, 3, 4)], var_cols[c(1, 3, 4)])) +
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3)) +
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))
ggsave(paste0(fig_path, fname, '_cls_boxplot.png'), height = 8, width = 14)

ggplot(events_som[cluster %in% c(cl_inc, cl_dec)], aes(p_dv_m, q_dv_m, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)])+
  scale_size_continuous(range = c(1, 6)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))
ggsave(paste0(fig_path, fname, '_pq.png'), height = 8, width = 14)

ggplot(events_som[cluster %in% c(cl_inc, cl_dec)], aes(q_dv_m, s_dv_m, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 6)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))
ggsave(paste0(fig_path, fname, '_qs.png'), height = 8, width = 14)

ggplot(events_som[cluster %in% c(cl_inc, cl_dec)], aes(x = start_q - start_p3, y = p_dv_m, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 8)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() + 
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))
ggsave(paste0(fig_path, fname, '_dstart_pq.png'), height = 8, width = 14)

ggplot(events_som[cluster %in% c(cl_inc, cl_dec)], aes(x = start_s - start_p3, y = p_dv_m, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 8)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))
ggsave(paste0(fig_path, fname, '_dstart_ps.png'), height = 8, width = 14)

ggplot(events_som[cluster %in% c(cl_inc, cl_dec)], aes(x = start_s - start_q, y = p_dv_m, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 8)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() +   
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))
ggsave(paste0(fig_path, fname, '_dstart_qs.png'), height = 8, width = 14)

























#############OTHER

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

