#Application of SOMs to find event clusters 
library(kohonen); library(tidyverse)
source('./source/functions.R'); source('./source/graphics.R') 

events <- readRDS('./data/events_met1.Rds') #created in init.R

map_dimension <- 10
n_iterations <- 10000
recalculate_map <- F
dr_dur <- 3

events_for_som <- events[(REG == 'CEU') & dur >= dr_dur & year(start) >= 1900,
                         .(ID, dur, start_month, x, y, year = year(start),
                           start_p3_month = month(start_p3), start_s_month = month(start_s), start_q_month = month(start_q),  
                           p_dv_m, pet_ev_m, s_dv_m, q_dv_m)]
events_for_som[is.na(start_p3_month), start_p3_month := 0]
events_for_som[is.na(start_q_month), start_q_month := 0]
events_for_som[is.na(start_s_month), start_s_month := 0]

som_grid <- somgrid(xdim = map_dimension, 
                    ydim = map_dimension, 
                    topo = "hexagonal")

results_path <- './results/som/ceu/'
fig_path <- './results/figs/som/ceu/'
fname <- paste0('som_start_', som_grid$xdim, '_', n_iterations, '_', dr_dur)
path_name <- paste0(results_path, fname, '.Rdata')

if(recalculate_map == F & file.exists(fname) == T){
  load(fname)
} else {
  m <- supersom(apply(events_for_som[, c('dur', 'start_p3_month', 'start_s_month', 'start_q_month')], 2, scale), 
                grid = som_grid, 
                rlen= n_iterations, 
                alpha = 0.05
                #,dist.fcts = distances
                #, user.weights = weight_layers
                #, maxNA.fraction = .5
  )
  save(m, file = path_name)
}

plot(m, type = "changes")

# generate distance matrix for codes
groups = 36
som_hc <- cutree(hclust(dist(m$codes[[1]])), groups)
plot(m, type="mapping", main = "Cluster Map", bgcol = palette_mid_qual(groups)[som_hc])
add.cluster.boundaries(m, som_hc)

cls = som_hc[m$unit.classif]

#events_for_som$cluster <- m$unit.classif #no clustering -> all soms used
events_for_som$cluster <- melt(som_hc[m$unit.classif])[, 1]
events_for_som[, n_clusters := .N, by = cluster]
events_som <- events[, .(start, ID, PT_ID, x, y, start_p3, start_q, start_s)][events_for_som, on = 'ID']
events_som[, n_cells := .N, by = year(start)]

som_sum <- events_som[, lapply(.SD, median, na.rm = T), by = cluster]
#som_sum_sd <- events_som[, lapply(.SD, sd, na.rm = T), by = cluster]
som_sum <- round(som_sum, 2)
#som_sum_sd <- round(som_sum_sd, 2)
som_sum
#som_sum_sd

boxplot(dur~cluster, events_for_som)
events_som_melt <- melt(events_som[, .(ID, PT_ID, x = scale(x), y = scale(y), year, dur, start_month, 
                                       start_p3_month, start_s_month, start_q_month,  p_dv_m, s_dv_m, q_dv_m, cluster)],
                        id.vars = c('ID', 'PT_ID', 'year','cluster'))

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

#annual starting month composition
ggplot(events_som, aes(x = year, fill = factor(start_month))) +
  geom_bar(position = position_stack()) +
  scale_fill_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  theme_bw()
ggsave(paste0(fig_path, fname, '_month_composite.png'), height = 8, width = 14)

#annual event composition
to_plot <- events_som[, .N, .(cluster, year)]
ggplot(to_plot[N > 50], aes(x = year, y = N, fill = factor(cluster))) +
  geom_bar(stat = 'identity', position = position_stack()) +
  scale_fill_manual(values = palette_mid_qual(36)) +
  theme_bw()
ggsave(paste0(fig_path, fname, '_cls_composite.png'), height = 8, width = 14)

#annual event composition trend
#to_plot <- to_plot[N > 50]
aa <- data.table(expand.grid(1:map_dimension^2, seq(min(to_plot$year), max(to_plot$year), 1)))
colnames(aa) = c('cluster', 'year')
to_plot <- to_plot[aa, on = c('year', 'cluster')]
to_plot[is.na(N), N := 0]
setorder(to_plot, year, cluster)

ggplot(to_plot, aes(x = year, y = N, col = factor(cluster), linetype = factor(cluster))) +
  #geom_point(alpha = 0.5) +
  geom_smooth(se = F, method = "lm") +
  scale_color_manual(values = palette_mid_qual(36)) +
  scale_linetype_manual(values = c(rep(1:7, 5), 1)) +  
  #geom_text(data = to_plot[year == "2017"], aes(label = cluster, col = cluster, x = Inf, y = N), hjust = -.1) +
  theme_bw() 

cluster_slope <- to_plot[, .(slope = list(lm(N~year, .SD)$coef[2])), by = cluster]
cluster_slope <- data.table(cluster_slope[, melt(slope)])
colnames(cluster_slope) = c('slope', 'cluster')
setorder(cluster_slope, slope)

cl_inc <- cluster_slope[slope > 0.04, cluster]
cl_dec <- cluster_slope[slope < -0.04, cluster]
som_sum[cluster %in% cl_inc, .(cluster, dur, start_p3_month, start_s_month, start_q_month, p_dv_m, s_dv_m, q_dv_m, n_clusters)]
som_sum[cluster %in% cl_dec, .(cluster, dur, start_p3_month, start_s_month, start_q_month, p_dv_m, s_dv_m, q_dv_m, n_clusters)]
to_plot[cluster %in% c(cl_inc, cl_dec), sum(N)]/to_plot[, sum(N)]

events_som[, slope := 'neutral']
events_som[cluster %in% cl_inc, slope := 'pos']
events_som[cluster %in% cl_dec, slope := 'neg']
save(events_som, file = paste0(results_path, fname, '_events.Rdata'))

ggplot(to_plot[cluster %in% c(cl_inc, cl_dec)], aes(x = year, y = N)) +
  geom_bar(stat = 'identity', fill = 'grey20') +
  geom_smooth(se = F, method = "lm", col = colset_mid[11]) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_linetype_manual(values = rep(1:5, 5)) +  
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() + 
  theme(strip.background = element_rect(fill = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey90'))
ggsave(paste0(fig_path, fname, '_cls_trends.png'), height = 8, width = 14)

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













