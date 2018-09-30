#Application of SOMs to find event clusters 
library(kohonen); library(tidyverse)
source('./source/functions.R'); source('./source/graphics.R') 

events <- readRDS('./data/events_met1.Rds') #created in init.R

map_dimension <- 5
n_iterations <- 10000
recalculate_map <- F
dr_dur <- 3
month_trans <- function(x) (sin(2 * pi * x/12)) #this tranformation is necessary for SOMs to describe the circularity in months

events_for_som <- events[dur >= dr_dur & year(start) >= 1900, #set region here
                         .(ID, dur, start_month, end_month, x, y, year = year(start),
                           start_p3_month = month(start_p3), start_s_month = month(start_s), start_q_month = month(start_q),  
                           p_dv_m, pet_ev_m, s_dv_m, q_dv_m)]
events_for_som[is.na(start_p3_month), start_p3_month := 0]
events_for_som[is.na(start_q_month), start_q_month := 0]
events_for_som[is.na(start_s_month), start_s_month := 0]

som_grid <- somgrid(xdim = map_dimension, 
                    ydim = map_dimension, 
                    topo = "hexagonal")

results_path <- './results/som/' #change save folder
fig_path <- './results/figs/som/' #change save folder
fname <- paste0('som_start_end_', som_grid$xdim, '_', n_iterations, '_', dr_dur)
path_name <- paste0(results_path, fname, '.Rdata')

dummy <- events_for_som[, .(dur, start_p3_month, start_s_month, start_q_month, end_month)]
dummy <- dummy[, dur := scale(dur)]
cols <-  c('start_p3_month', 'start_s_month', 'start_q_month', 'end_month')
dummy[ , (cols) := lapply(.SD, month_trans), .SDcols = cols]
dummy <- as.matrix(dummy)

if(recalculate_map == F & file.exists(path_name) == T){
  load(path_name)
} else {
  m <- supersom(dummy, 
                grid = som_grid, 
                rlen= n_iterations, 
                alpha = 0.05
                #,dist.fcts = distances
                #, user.weights = weight_layers
                #, maxNA.fraction = .5
  )
  save(m, file = path_name)
}

# generate distance matrix for codes
#groups = 20
#som_hc <- cutree(hclust(dist(m$codes[[1]])), groups)
#plot(m, type="mapping", main = "Cluster Map", bgcol = palette_mid_qual(groups)[som_hc])
#add.cluster.boundaries(m, som_hc)
#cls = som_hc[m$unit.classif]
#events_for_som$cluster <- melt(som_hc[m$unit.classif])[, 1]

events_for_som$cluster <- m$unit.classif #no clustering -> all soms used
events_for_som[, n_clusters := .N, by = cluster]
events_som <- events[, .(start, ID, PT_ID, x, y, start_p3, start_q, start_s)][events_for_som, on = 'ID']
events_som[, n_cells := .N, by = year(start)]

som_sum <- events_som[, lapply(.SD, median, na.rm = T), by = cluster]
som_sum <- round(som_sum, 2)
som_sum

events_som_melt <- melt(events_som[, .(ID, PT_ID, x = scale(x), y = scale(y), year, dur, start_month, 
                                       start_p3_month, start_s_month, start_q_month,  p_dv_m, s_dv_m, q_dv_m, cluster)],
                        id.vars = c('ID', 'PT_ID', 'year','cluster'))

n_cls_yr <- events_som[, .N, .(cluster, year)]
aa <- data.table(expand.grid(1:map_dimension^2, seq(min(n_cls_yr$year), max(n_cls_yr$year), 1)))
colnames(aa) = c('cluster', 'year')
n_cls_yr <- n_cls_yr[aa, on = c('year', 'cluster')]
n_cls_yr[is.na(N), N := 0]
setorder(n_cls_yr, year, cluster)
cluster_slope <- n_cls_yr[, .(slope = list(lm(N~year, .SD)$coef[2])), by = cluster]
cluster_slope <- data.table(cluster_slope[, melt(slope)])
colnames(cluster_slope) = c('slope', 'cluster')
setorder(cluster_slope, slope)

cl_inc <- cluster_slope[slope > 0.10, cluster] #change threshold to get most significant changes
cl_dec <- cluster_slope[slope < -0.30, cluster]  #change threshold to get most significant changes
som_sum[cluster %in% cl_inc, .(cluster, dur, start_p3_month, start_s_month, start_q_month, p_dv_m, s_dv_m, q_dv_m, n_clusters)]
som_sum[cluster %in% cl_dec, .(cluster, dur, start_p3_month, start_s_month, start_q_month, p_dv_m, s_dv_m, q_dv_m, n_clusters)]
n_cls_yr[cluster %in% c(cl_inc, cl_dec), sum(N)]/n_cls_yr[, sum(N)]
events_som[, slope := 'neutral']
events_som[cluster %in% cl_inc, slope := 'pos']
events_som[cluster %in% cl_dec, slope := 'neg']
events_som[, period := cut(year, 3,  c('1900-1939', '1940-1978', '1979-2018'))]
events_som[, dur_cat := cut(dur, c(0, 6, 12, 100), c('3-6', '7-12', '>12'))]
save(events_som, cl_inc, cl_dec, file = paste0(results_path, fname, '_events.Rdata'))

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
  labs(x = 'Cluster', y = 'Size') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 
ggsave(paste0(fig_path, fname, '_start_month_cls.png'), plot = gg_start, height = 8, width = 14)

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
  labs(x = 'Cluster', y = 'Size') + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(~period, ncol = 3) +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) 
ggsave(paste0(fig_path, fname, '_end_month_cls.png'), plot = gg_end, height = 8, width = 14)

###Duration of annual droughts
gg_dur_12 <- ggplot(events_som[n_clusters > 500 & dur <= 12], aes(x = factor(cluster), fill = factor(dur))) +
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
ggsave(paste0(fig_path, fname, '_dur_12_cls.png'), plot = gg_dur_12, height = 8, width = 14)

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
  geom_bar(stat = 'identity', fill = 'grey20') +
  geom_smooth(se = F, method = "lm", col = colset_mid[11]) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
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













