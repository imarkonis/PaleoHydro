library(data.table); library(ggplot2)

mhm <- readRDS('./data/timeseries_met1.Rds')
coords <- readRDS('./data/spatial.Rds')

tree_rings <- fread('./data/raw/Carp_plot_geo.txt')
tree_rings[, V1 := NULL]
tree_rings <- tree_rings[foresttype == 'spruce']
setnames(tree_rings, "lng", "lon")

coords[, lon := round(lon, 0)]
coords[, lat := round(lat, 0)]

tree_rings[, lon := round(lon, 0)]
tree_rings[, lat := round(lat, 0)]


coords_to_merge <- coords[, .(PT_ID, lon, lat)]
coords_to_merge[tree_rings, on = .(lat, lon)]


coords_to_merge[lat == 47.1 & lon == 25.3]

mhm_trees <- mhm[PT_ID %in% c(1066, 970, 875, 829)]
mhm_trees[, PT_ID := factor(PT_ID, labels = c('SLO', "UKR", "ROM", "FAG"))]
saveRDS(mhm_trees, file = 'mhm_trees.rds')
mhm_trees <- readRDS(file = 'mhm_trees.rds')


ggplot(mhm[PT_ID ==  1066, .(DTM, s)], aes(DTM, s)) +
  geom_line()
ggplot(mhm[PT_ID ==  970, .(DTM, s)], aes(DTM, s)) +
  geom_line()
ggplot(mhm[PT_ID ==  875, .(DTM, s)], aes(DTM, s)) +
  geom_line()
ggplot(mhm[PT_ID == 829, .(DTM, s)], aes(DTM, s)) +
  geom_line()

ggplot(mhm[PT_ID == 795, .(DTM, s)], aes(DTM, s)) +
  geom_line()

