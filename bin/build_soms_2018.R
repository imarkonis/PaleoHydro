#Application of SOMs to find event clusters using only the first 7 months of year (2018 comparison)
library(kohonen); library(tidyverse)
source('./source/functions.R'); source('./source/graphics.R') 

dtm <- readRDS('./data/mstat_short_met1.Rds') #main analysis dataset
dtm_sp <- readRDS('./data/spatial.Rds')

events <- dtm[!is.na(ID) & !is.na(REG) & month(DTM) <= 7 & (REG == 'CEU' | REG == 'NEU') & dur >= 3 & year(start) >= 1900, 
              .(REG, PT_ID, ID, start, start_month, dur, start_s, start_q, start_p3, p_dv, q_dv, s_dv, pet_ev, t_ev)]
events[, start_s := max(start_s, na.rm = T), .(PT_ID, ID)]
events[, start_q := max(start_q, na.rm = T), .(PT_ID, ID)]
events[, start_p3 := max(start_p3, na.rm = T), .(PT_ID, ID)]
events[, p_dv_m := mean(p_dv, na.rm = T), ID]
events[, q_dv_m := mean(q_dv, na.rm = T), ID]
events[, s_dv_m := mean(s_dv, na.rm = T), ID]
events[, pet_ev_m := mean(pet_ev, na.rm = T), ID]
events[, t_ev_m := mean(t_ev, na.rm = T), ID]
events[, p_dv := NULL]      
events[, q_dv := NULL]      
events[, s_dv := NULL]      
events[, t_ev := NULL]      
events[, pet_ev := NULL]      
events <- events[!duplicated(events)]
events[is.na(p_dv_m), p_dv_m := 0]
events[is.na(q_dv_m), q_dv_m := 0]
events[is.na(s_dv_m), s_dv_m := 0]
events[is.na(pet_ev_m), pet_ev_m := 0]
events[is.na(t_ev_m), t_ev_m := 0]
events <- merge(dtm_sp[, .(PT_ID, x, y)], events, by = 'PT_ID')


map_dimension <- 10
n_iterations <- 1000
recalculate_map <- F

events_for_som <- events[,.(ID, dur, start_month, x, y, year = year(start),
                           start_p3_month = month(start_p3), start_s_month = month(start_s), start_q_month = month(start_q),  
                           p_dv_m, pet_ev_m, s_dv_m, q_dv_m)]
events_for_som[is.na(start_p3_month), start_p3_month := 0]
events_for_som[is.na(start_q_month), start_q_month := 0]
events_for_som[is.na(start_s_month), start_s_month := 0]

som_grid <- somgrid(xdim = map_dimension, 
                   ydim = map_dimension, 
                   topo = "hexagonal")

results_path <- './results/som/'
fname <- paste0('som_start_space_2018_', som_grid$xdim, '_', n_iterations)
path_name <- paste0(results_path, fname, '.Rdata')
if(recalculate_map == F & file.exists(fname) == T){
  load(fname)
} else {
  m <- supersom(apply(events_for_som[, c('x', 'y', 'dur','year', 'start_p3_month', 'start_s_month', 'start_q_month')], 2, scale), 
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
plot(m, type = "counts")
plot(m, type = "codes")

# generate distance matrix for codes
groups = 30
som_hc <- cutree(hclust(dist(m$codes[[1]])), groups)
plot(m, type="mapping", main = "Cluster Map", bgcol = palette_mid_qual(groups)[som_hc])
add.cluster.boundaries(m, som_hc)

cls = som_hc[m$unit.classif]

events_for_som$cluster <- melt(som_hc[m$unit.classif])[, 1]
events_for_som[, n_clusters := .N, by = cluster]
events_som <- events[, .(start, ID, PT_ID, x, y, start_p3, start_q, start_s)][events_for_som, on = 'ID']
events_som[, n_cells := .N, by = year(start)]

som_sum <- events_som[, lapply(.SD, median, na.rm = T), by = cluster]
som_sum_sd <- events_som[, lapply(.SD, sd, na.rm = T), by = cluster]
som_sum <- round(som_sum[n_clusters > 100, ], 2)
som_sum_sd <- round(som_sum_sd, 2)
som_sum
som_sum_sd

boxplot(dur~cluster, events_for_som)

events_som_melt <- melt(events_som[, .(ID, PT_ID, x = scale(x), y = scale(y), year, dur, start_month, 
                                       start_p3_month, start_s_month, start_q_month,  p_dv_m, s_dv_m, q_dv_m, cluster)],
                       id.vars = c('ID', 'PT_ID', 'year','cluster'))

#clusters in space (map)
ggplot(events_som[n_clusters > 100], aes(x, y, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  facet_wrap(~cluster) +
  theme_bw()
ggsave(paste0('./results/figs/som/', fname, '_map.png'), height = 8, width = 14)

#cluster properties (boxplot)
ggplot(events_som_melt, aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.3))
ggsave(paste0('./results/figs/som/', fname, '_cls_boxplot.png'), height = 8, width = 14)

#annual starting month composition
ggplot(events_som, aes(x = year, fill = factor(start_month))) +
  geom_bar(position = position_stack()) +
  scale_fill_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  theme_bw()
ggsave(paste0('./results/figs/som/', fname, '_month_composite.png'), height = 8, width = 14)

#annual event composition
ggplot(events_som[n_clusters > 300], aes(x = year(start), fill = factor(cluster))) +
  geom_bar(position = position_stack()) +
  scale_fill_manual(values = palette_mid_qual(32)) +
  theme_bw()
ggsave(paste0('./results/figs/som/', fname, '_cls_composite.png'), height = 8, width = 14)

to_plot <- events_som[, .N, .(cluster, year)]
ggplot(to_plot[N > 50], aes(x = year, y = N, fill = factor(cluster))) +
  geom_bar(stat = 'identity', position = position_stack()) +
  scale_fill_manual(values = palette_mid_qual(32)) +
  theme_bw()
to_plot <- to_plot[N > 50]
setorder(to_plot, year)
table(to_plot[1:100]$cluster)
table(to_plot[336:436]$cluster)
som_sum[cluster %in% c(3, 10, 28), .(cluster, dur, start_p3_month, start_s_month, start_q_month, p_dv_m, s_dv_m, q_dv_m)]
som_sum[cluster %in% c(4, 9, 22, 30), .(cluster, dur, start_p3_month, start_s_month, start_q_month, p_dv_m, s_dv_m, q_dv_m)]

to_plot <- events_som[n_cells > 200 & n_clusters > 100, .(start = year(start), cluster)]
to_plot[, n_cls := .N, .(start, cluster)]
to_plot <- to_plot[!duplicated(to_plot)]
ggplot(to_plot, aes(x = start, y = n_cls, col = factor(cluster))) +
  #geom_point(alpha = 0.5) +
  geom_smooth(se = F, method = "lm") +
  scale_color_manual(values = palette_mid_qual(32)) +
  theme_bw()

ggplot(events_som[n_clusters > 100], aes(p, q, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)])+
  scale_size_continuous(range = c(1, 6)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n_clusters > 100], aes(q, s, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 6)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n_clusters > 100], aes(pet, s, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 6)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()



ggplot(events_som[n_clusters > 100], aes(x = start_s - start_p3, y = p, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 8)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n_clusters > 100], aes(x = start_s - start_q, y = p, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 8)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n_clusters > 100 & n < 2000], aes(x = start_s - start_q, y = s, col = factor(year(start)),
                                shape = factor(year(start)))) +
  geom_point(size = 3, alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = palette_mid(119)) +
  scale_shape_manual(values = c(rep(1:25, 4), 1:19)) +
  facet_wrap(~cluster) +
  theme_bw()













