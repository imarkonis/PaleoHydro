source('./source/functions.R'); source('./source/graphics.R') 

load('./results/som/ceu/som_start_end_5_10000_3_events.Rdata')
coords <- readRDS('./data/spatial.Rds')
events_som <- coords[events_som, on = 'PT_ID']

test <- events_som[, .(PT_ID, cluster, year, lon, lat)]
mean(nndist(c(test[year == 1900]$x, test[year == 1900]$y)))
mean(nndist(c(test[year == 1901]$x, test[year == 1901]$y)))

test[, n_cluster := .N, .(cluster, year)]
test[, nndist_m := mean(nndist(x, y)), .(cluster, year)]
test[, nndist_m_yr := mean(nndist(x, y)), .(year)]

ggplot(test[n_cluster > 10], aes(year, nndist_m)) +
  geom_line() +
  geom_point(size = test[n_cluster > 10]$n_cluster/100) +
  facet_wrap(~cluster) +
  theme_bw()

ggplot(test[year == 2012], aes(lon, lat, col = factor(cluster))) +
  scale_color_manual(values = palette_mid(25)) +
  geom_point() +
  facet_wrap(~cluster) +
  theme_bw()

