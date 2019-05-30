source('./source/functions.R'); source('./source/graphics.R') 
library(factoextra)

load("./results/som/ceu/som_for_tree.Rdata")


# generate distance matrix for codes
groups <- 8
som_hc <- hclust(dist(m$codes[[1]]))
plot(m, type="mapping", main = "Cluster Map", bgcol = palette_mid_qual(groups)[som_hc])
#add.cluster.boundaries(m, som_hc)
#cls = som_hc[m$unit.classif]
#events_for_som$cluster <- melt(som_hc[m$unit.classif])[, 1]

aa <- unique(events_som[, .(cluster, hclust)])
fviz_dend(som_hc, k = 8,                 # Cut in four groups
          cex = 0.5,                 # label size
          k_colors = palette_mid_qual(8),
          color_labels_by_k = TRUE,  # color labels by groups
          ggtheme = theme_minimal()     # Change theme
)

fviz_dend(som_hc, cex = 0.5)

fviz_dend(som_hc, cex = 0.5, 
          main = "Dendrogram - ward.D2",
          xlab = "Objects", ylab = "Distance", sub = "")

dend <- as.dendrogram(som_hc)
plot(dend)
