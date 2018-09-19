#Application of Random Forests to find event clusters: see https://rpubs.com/erblast/SOM for factors
library(kohonen); library(tidyverse)
source('./source/functions.R'); source('./source/graphics.R') 

events <- readRDS('./data/events_met1.Rds') #created in init.R

map_dimension <- 10
n_iterations <- 10000
recalculate_map <- T


events_for_som <- events[REG == 'CEU' & dur >= 4 & year(start) >= 1900, 
                         .(ID, dur, start_month, 
                           start_p3_month = month(start_p3), start_s_month = month(start_s), start_q_month = month(start_q),  
                           p, s, q)]
events_for_som[is.na(p), p := 0]
events_for_som[is.na(q), q := 0]
events_for_som[is.na(s), s := 0]
events_for_som[is.na(start_p3_month), start_p3_month := 0]
events_for_som[is.na(start_q_month), start_q_month := 0]
events_for_som[is.na(start_s_month), start_s_month := 0]

som_grid <- somgrid(xdim = map_dimension, 
                   ydim = map_dimension, 
                   topo = "hexagonal")

fname <- paste0('./results/som/som_dv_start_', som_grid$xdim, '_', n_iterations, '.Rdata')
if(recalculate_map == F & file.exists(fname) == T){
  load(fname)
} else {
  m <- supersom(apply(events_for_som[, c('dur', 'p', 'q', 's', 'start_p3_month', 'start_s_month', 'start_q_month')], 2, scale), 
                grid = som_grid, 
                rlen= n_iterations, 
                alpha = 0.05
                #,dist.fcts = distances
                         #, user.weights = weight_layers
                         #, maxNA.fraction = .5
  )
  save(m, file = fname)
}

plot(m, type = "changes")
plot(m, type = "counts")
plot(m, type = "codes")

# generate distance matrix for codes
groups = 32
som_hc <- cutree(hclust(dist(m$codes[[1]])), groups)
plot(m, type="mapping", main = "Cluster Map", bgcol = palette_mid_qual(groups)[som_hc])
add.cluster.boundaries(m, som_hc)

cls = som_hc[m$unit.classif]

events_for_som$cluster <- melt(som_hc[m$unit.classif])[, 1]
events_for_som[, n := .N, by = cluster]

events_som <- events[, .(start, ID, PT_ID, x, y, start_p3, start_q, start_s)][events_for_som, on = 'ID']

boxplot(dur~cluster, events_for_som)

som_sum <- events_som[, lapply(.SD, median, na.rm = T), by = cluster]
som_sum_sd <- events_som[, lapply(.SD, sd, na.rm = T), by = cluster]
som_sum <- round(som_sum[n > 100, ], 2)
som_sum_sd <- round(som_sum_sd, 2)
som_sum
som_sum_sd

ggplot(events_som[n > 100], aes(x = year(start))) +
  geom_bar() +
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n > 100], aes(p, q, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)])+
  scale_size_continuous(range = c(1, 6)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n > 100], aes(q, s, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 6)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n > 100 & y < 3500000], aes(x, y, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  facet_wrap(~cluster) +
  theme_bw()

ggplot(events_som[n > 100], aes(x = start_s - start_p3, y = p, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 8)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n > 100], aes(x = start_s - start_q, y = p, col = factor(start_month), size = dur)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_size_continuous(range = c(1, 8)) + 
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()

ggplot(events_som[n > 100 & n < 2000], aes(x = start_s - start_q, y = s, col = factor(year(start)),
                                shape = factor(year(start)))) +
  geom_point(size = 3, alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = palette_mid(119)) +
  scale_shape_manual(values = c(rep(1:25, 4), 1:19)) +
  facet_wrap(~cluster) +
  theme_bw()

ggplot(events_som[n > 100 & s > 1], aes(x = year(start), fill = factor(cluster))) +
  geom_bar(position = position_stack()) +
  scale_fill_manual(values = palette_mid_qual(26)) +
  theme_bw()

ggplot(events_som[n > 100 & s > 1], aes(x = year(start), fill = factor(start_month))) +
  geom_bar(position = position_stack()) +
  scale_fill_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  theme_bw()









