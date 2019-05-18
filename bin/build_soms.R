#Application of SOMs to find event clusters 
library(kohonen); library(tidyverse)
source('./source/functions.R'); source('./source/graphics.R') 

events <- readRDS('./data/events_met1.Rds') #created in init.R

map_dimension <- 5
n_iterations <- 10000
recalculate_map <- F
dr_dur <- 3
month_trans <- function(x) (sin(2 * pi * x/12)) #this tranformation is necessary for SOMs to describe the circularity in months

events_for_som <- events[REG == 'CEU' & dur >= dr_dur & year(start) >= 1900, #set region here
                         .(ID, dur, year = year(start),
                           end_month = month(end),
                           start_p3_month = month(start_p3), 
                           start_s_month = month(start_s), 
                           start_q_month = month(start_q),  
                           p_dv_m, pet_ev_m, s_dv_m, q_dv_m)]
events_for_som[is.na(start_p3_month), start_p3_month := 0]
events_for_som[is.na(start_q_month), start_q_month := 0]
events_for_som[is.na(start_s_month), start_s_month := 0]
events_for_som <- unique(events_for_som)
som_grid <- somgrid(xdim = map_dimension, 
                    ydim = map_dimension, 
                    topo = "hexagonal")

results_path <- './results/som/ceu/' #change save folder
fig_path <- './results/figs/som/ceu/' #change save folder
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
#groups = 8
#som_hc <- cutree(hclust(dist(m$codes[[1]])), groups)
#plot(m, type="mapping", main = "Cluster Map", bgcol = palette_mid_qual(groups)[som_hc])
#add.cluster.boundaries(m, som_hc)
#cls = som_hc[m$unit.classif]
#events_for_som$cluster <- melt(som_hc[m$unit.classif])[, 1]

events_for_som$cluster <- m$unit.classif #no clustering -> all soms used
#events_for_som[, n_clusters := .N, by = cluster]
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

cl_inc <- cluster_slope[slope > 0.8, cluster] #change threshold to get most significant changes
cl_dec <- cluster_slope[slope < -0.8, cluster]  #change threshold to get most significant changes
som_sum[cluster %in% cl_inc, .(cluster, dur, start_p3_month, start_s_month, start_q_month, p_dv_m, s_dv_m, q_dv_m, n_clusters)]
som_sum[cluster %in% cl_dec, .(cluster, dur, start_p3_month, start_s_month, start_q_month, p_dv_m, s_dv_m, q_dv_m, n_clusters)]
n_cls_yr[cluster %in% c(cl_inc, cl_dec), sum(N)]/n_cls_yr[, sum(N)]
events_som[, slope := 'neutral']
events_som[cluster %in% cl_inc, slope := 'pos']
events_som[cluster %in% cl_dec, slope := 'neg']
events_som[, period := cut(year, 3,  c('1900-1939', '1940-1978', '1979-2018'))]
events_som[, dur_cat := cut(dur, c(0, 6, 12, 100), c('3-6', '7-12', '>12'))]
save(events_som, cl_inc, cl_dec, n_cls_yr, file = paste0(results_path, fname, '_events.Rdata'))

