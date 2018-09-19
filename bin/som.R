#Application of Random Forests to find event clusters
library(kohonen); library(tidyverse)
source('./source/functions.R'); source('./source/graphics.R') 

events <- readRDS('./data/events_met1.Rds') #created in init.R

map_dimension <- 8
n_iterations <- 1000
recalculate_map <- 'T'

events_for_som <- events[REG == 'CEU' & dur >= 4, 
                         .(ID, dur, start_month, 
                           start_p3 = month(start_p3), start_s = month(start_s), start_q = month(start_q),  
                           p, s, q)]
#events_for_som[, start_s := factor(month(start_s))]
#events_for_som[, start_q := factor(month(start_q))]
#events_for_som[is.na(start_q), start_q := 'not']
#events_for_som[is.na(start_s), start_s := 'not']
events_for_som[is.na(p), p := 0]
events_for_som[is.na(q), q := 0]
events_for_som[is.na(s), s := 0]

som_grid <- somgrid(xdim = map_dimension, 
                   ydim = map_dimension, 
                   topo = "hexagonal")

if(recalculate_map == F & file.exists('./results/som.Rdata') == T){
  load('./results/som.Rdata')
} else {
  m <- supersom(apply(events_for_som[, 2:6], 2, scale), 
                grid = som_grid, 
                rlen= n_iterations, 
                alpha = 0.05
                #whatmap = colnames(events_for_som)
                #,dist.fcts = distances
                         #, user.weights = weight_layers
                         #, maxNA.fraction = .5
  )
  save(m, file = './results/som.Rdata')
}

plot(m, type="changes")
plot(m, type="counts")
plot(m, type="codes")

# generate distance matrix for codes
groups = 16
som_hc <- cutree(hclust(dist(m$codes[[1]])), groups)
plot(m, type="mapping", main = "Cluster Map", bgcol = palette_light_qual(groups)[som_hc])
add.cluster.boundaries(m, som_hc)

cls = som_hc[m$unit.classif]

events_for_som$cluster <- melt(som_hc[m$unit.classif])[, 1]
events_for_som[, n := .N, by = cluster]

events_som <- events[, .(start, ID, PT_ID)][events_for_som, on = 'ID']

boxplot(dur~cluster, events_for_som)

som_sum <- events_som[, lapply(.SD, median, na.rm = T), by = cluster]
som_sum_sd <- events_som[, lapply(.SD, sd, na.rm = T), by = cluster]
som_sum <- round(som_sum[n > 100, ], 2)
som_sum_sd <- round(som_sum_sd, 2)
som_sum
som_sum_sd

ggplot(events_som, aes(year(start))) +
  geom_bar() +
  facet_wrap(~cluster, scales = 'free_y') +
  theme_bw()











