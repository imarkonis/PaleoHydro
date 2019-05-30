### Explore the changes in the clusters over 3 periods

source('./source/functions.R'); source('./source/graphics.R')

dta <- readRDS('./data/timeseries_met1.Rds')
events <- readRDS('./data/events_met1.Rds')
filename <- 'som_start_end_5_10000_3_events.Rdata'
fig_path <- './results/figs/'
results_path <- './results/som/ceu/'
load(paste0(results_path, filename)) 
events_ceu <- events_som
results_path <- './results/som/neu/'
load(paste0(results_path, filename)) 
events_neu <- events_som
results_path <- './results/som/med/'
load(paste0(results_path, filename)) 
events_med <- events_som
events_som <-rbind(events_neu, events_ceu, events_med)
aa <- unique(events[, .(ID, REG, p3_dur_tot, s_dur_tot, q_dur_tot)])
events_som <- aa[events_som, on = 'ID']
events_som[, cluster := paste0(REG, '_SOM_', cluster)]
events_som[, hclust := paste0(REG, '_', hclust)]
saveRDS(events_som, './data/events_som.Rds')
events_som <- readRDS('./data/events_som.Rds')

timeseries <- dta[, .(PT_ID, DTM, ID, p_dv, q_dv, s_dv, pet_ev)]
timeseries <- melt(timeseries, id.vars = c('PT_ID', 'DTM', 'ID'))

### Number of grid cells with drought per month in the 3 periods
aa <- unique(events_som[, .(ID, period, dur_cat, REG)])
timeseries_all <- aa[timeseries, on = 'ID']
timeseries_all <- timeseries_all[complete.cases(timeseries_all)]

to_plot <- timeseries_all[, .N, .(month = month(DTM), 
                                  period, 
                                  variable = factor(variable),
                                  region = REG)]
to_plot[, mean_N := as.integer(mean(N)), .(period, variable, region)]
to_plot$region = factor(to_plot$region, levels = c("NEU", "CEU", "MED"))
                        
gg_change <- ggplot(to_plot, aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  geom_rug(aes(x = month, y = mean_N, col = variable), sides = "rl", size = 2) +
  geom_line(aes(x = month, y = mean_N, col = variable), size = 2, alpha = 0.3) +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(region ~ period) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Grid cells') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme(panel.spacing.x = unit(0, "lines")) +
  theme_opts
ggsave(paste0('./results/figs/drought_change.png'), plot = gg_change, height = 8, width = 10)

### Drought main types as determined by hclust
events_som[, n_hclust := .N, hclust]
aa <- unique(events_som[n_hclust > 7000, .(ID, period, hclust, cluster, REG)])
timeseries_all <- aa[timeseries, on = 'ID']
timeseries_all <- timeseries_all[complete.cases(timeseries_all)]

to_plot <- timeseries_all[, .N, .(
  month = month(DTM),
  cluster = factor(hclust),
  variable = factor(variable),
  region = REG
)]
events_som[hcl_slope > 0, hcl_slope_qual := 'pos', hclust]
events_som[hcl_slope < 0, hcl_slope_qual := 'neg', hclust]
table(events_som[, .(hclust, hcl_slope_qual)])
table(events_som[, .(hclust, slope)])
hcls_slopes_tab <- unique(events_som[, .(hclust, hcl_slope)])
hcls_slopes_tab[hcl_slope < -0.05]
hcls_slopes_tab[hcl_slope > 0.05]

sig_clusters <- c('NEU_3', 'NEU_6', 'NEU_2',
                  'CEU_1', 'CEU_8', 'CEU_4', 
  'MED_7', 'MED_3', 'MED_8')

to_plot$cluster = factor(to_plot$cluster, levels = sig_clusters)

plot_labeller <- function(variable, value){
  return(var_names[value])
}

var_names <- list(
  'NEU_3' = "Class A-NEU",
  'NEU_6' = "Class B-NEU",
  'NEU_2' = "Class C-NEU",
  'CEU_1' = "Class A-CEU",
  'CEU_8' = "Class B-CEU",
  'CEU_4' = "Class C-CEU",
  'MED_7' = "Class A-MED",
  'MED_3' = "Class B-MED",
  'MED_8' = "Class C-MED")

g1 <- ggplot(to_plot, aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~cluster, ncol = 3, labeller = plot_labeller) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Grid cells') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts  

#Not used in final plot--------------------------
g <- ggplot_gtable(ggplot_build(g))  #Change facet colors
strips <- which(grepl('strip-', g$layout$name))
pal <- c(var_cols[5], var_cols[1], var_cols[1], var_cols[4], var_cols[5], 
         var_cols[1], var_cols[4], var_cols[5],var_cols[1])
cols <- c( 'black','white', 'white', 'white', 'black', 'white',  'white', 'black', 'white') 

for (i in seq_along(strips)) {
  k <- which(grepl('rect', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
  l <- which(grepl('titleGrob', g$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
  g$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- pal[i]
  g$grobs[[strips[i]]]$grobs[[1]]$children[[l]]$children[[1]]$gp$col <- cols[i]
}

png('./results/figs/hcl_change.png', height = 15, width = 22, units = 'cm', res = 320)
grid::grid.draw(g)
dev.off()
#--------------------------------------------------

#slopes

to_plot <- events_som[hclust %in% sig_clusters, .(year, ID, hclust)]
to_plot$hclust = factor(to_plot$hclust, levels = sig_clusters)
to_plot[, N := .N, .(year, hclust)]
to_plot <- to_plot[complete.cases(to_plot)]
to_plot[, ID := NULL]
to_plot <- unique(to_plot)

g2 <- ggplot(to_plot, aes(x = as.numeric(year), y = N)) +
  geom_bar(stat="identity", fill = 'grey40') +
  geom_line(stat="smooth", method = "lm", col = 'dark orange', lwd = 1) +
  geom_smooth(data = to_plot[year >= 1960], se = F, method = "lm", col = colset_mid[11]) +
  scale_color_manual(values = colset_mid[c(1:2, 4:5, 8:11, 6:7, 12, 3)]) +
  scale_fill_manual(values = palette_mid(5)) +
  scale_linetype_manual(values = rep(1:5, 5)) +  
  facet_wrap(~hclust, labeller = plot_labeller) +
  theme_bw() + 
  labs(x = "Year", y = "Drought events") +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80')) +
  theme_opts  

gg_all <- ggarrange(g1,
                    g2,  labels = c("a", "b"), 
                    common.legend = T, legend = 'right', 
                    nrow = 1, ncol = 2)
ggsave(paste0('./results/figs/classes.png'), plot = gg_all, height = 6, width = 12)


#The heat-wave flash droughts in detail
aa <- unique(events_som[hclust %in% c('CEU_1', 'CEU_8', 'MED_7', 'MED_3', 'NEU_3', 'NEU_6') & slope == 'pos' & n_clusters > 1000, .(ID, period, hclust, cluster, REG)])
timeseries_all <- aa[timeseries, on = 'ID']
timeseries_all <- timeseries_all[complete.cases(timeseries_all)]

to_plot <- timeseries_all[, .N, .(
  month = month(DTM),
  cluster = factor(cluster),
  variable = factor(variable),
  region = REG
)]

ggplot(to_plot, aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~cluster, ncol = 3) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Grid cells') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts

aa <- unique(events_som[hclust %in% c('CEU_1', 'CEU_8', 'MED_7', 'MED_3', 'NEU_3', 'NEU_6') & slope == 'neg' & n_clusters > 1000, .(ID, period, hclust, cluster, REG)])
timeseries_all <- aa[timeseries, on = 'ID']
timeseries_all <- timeseries_all[complete.cases(timeseries_all)]


to_plot <- timeseries_all[, .N, .(
  month = month(DTM),
  cluster = factor(cluster),
  variable = factor(variable),
  region = REG
)]

ggplot(to_plot, aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~cluster, ncol = 3) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Grid cells') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts

#The heat-wave flash droughts in NEU
aa <- unique(events_som[hclust %in% c('NEU_3', 'NEU_6') & n_clusters > 1000, .(ID, period, hclust, cluster, REG)])
timeseries_all <- aa[timeseries, on = 'ID']
timeseries_all <- timeseries_all[complete.cases(timeseries_all)]

to_plot <- timeseries_all[cluster == "NEU_SOM_9" | cluster == "NEU_SOM_10" | 
                          cluster == "NEU_SOM_12" | cluster == "NEU_SOM_14", .N, .(
  month = month(DTM),
  cluster = factor(cluster),
  variable = factor(variable),
  region = REG
)]

to_plot[, cluster := factor(cluster, levels = c('NEU_SOM_9', 'NEU_SOM_10',
                            'NEU_SOM_12', 'NEU_SOM_14'))]

ggplot(to_plot, aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~cluster, ncol = 2) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Grid cells') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts

ggsave(paste0('./results/figs/NEU_flash_droughts.png'), height = 5, width = 6)

###Flash drought type composition
to_plot <- melt(events_som[slope == 'pos' & n_clusters > 1000 & 
                             hclust %in% c('CEU_1', 'MED_7', 'NEU_3'), 
                           .(hclust, p3 = p3_dur_tot/dur, q = q_dur_tot/dur, s = s_dur_tot/dur)],  
                id.vars = 'hclust')
to_plot <- to_plot[, mean(value), .(hclust, variable)]
colnames(to_plot)[3] <- 'value'
to_plot_2 <- to_plot
to_plot_2$value <- 1 - to_plot$value
to_plot_2$drought <- factor(paste0(to_plot$variable, '_no_dr'))
to_plot$drought <- factor(paste0(to_plot$variable, '_dr'))
to_plot <- rbind(to_plot, to_plot_2)
to_plot$hclust <- factor(to_plot$hclust)
levels(to_plot$hclust) <- c("CEU", "MED", "NEU")

g1 <- ggplot(data = as.data.table(to_plot), aes(x = variable, y = value, fill = drought)) +
  geom_bar(stat = "identity", position = position_fill(), width = 1, color = 'grey20') +
  facet_wrap(~hclust, nrow = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(var_cols[1:3], var_cols_alpha), breaks = c("Met.", "Hyd.", "Agr.")) +
  theme_void() +
  theme(aspect.ratio = 1) +
#  theme(strip.text.x = element_blank()) +
  guides(fill = guide_legend(title = "Drought Type"))

to_plot <- melt(events_som[slope == 'neg' & n_clusters > 1000 & 
                             hclust %in% c('CEU_1', 'MED_7', 'NEU_3'), 
                           .(hclust, p3 = p3_dur_tot/dur, q = q_dur_tot/dur, s = s_dur_tot/dur)],  
                id.vars = 'hclust')
to_plot <- to_plot[, mean(value), .(hclust, variable)]
colnames(to_plot)[3] <- 'value'
to_plot_2 <- to_plot
to_plot_2$value <- 1 - to_plot$value
to_plot_2$drought <- factor(paste0(to_plot$variable, '_no_dr'))
to_plot$drought <- factor(paste0(to_plot$variable, '_dr'))
to_plot <- rbind(to_plot, to_plot_2)
to_plot$hclust <- factor(to_plot$hclust)
levels(to_plot$hclust) <- c("CEU", "MED", "NEU")

g2 <- ggplot(data = as.data.table(to_plot), aes(x = variable, y = value, fill = factor(drought))) +
  geom_bar(stat = "identity", position = position_fill(), width = 1, color = 'grey20') +
  facet_wrap(~hclust, nrow = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c(var_cols[1:3], var_cols_alpha), breaks = c("Met.", "Hyd.", "Agr.")) +
  theme_void() +
  theme(aspect.ratio = 1) +
  guides(fill = guide_legend(title = "Drought Type"))

gg_all <- ggarrange(g2,
                    g1,  labels = c("a", "b"), 
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 1)
ggsave(paste0('./results/figs/flash_dr_composition.png'), plot = gg_all, height = 5, width = 10)


###Flash drought intensity (deficit volumes)
to_plot <- melt(events_som[hclust %in% c('CEU_1', 'MED_7', 'NEU_3'), .(p = p_dv_m, q = q_dv_m, slope, s = s_dv_m, n_clusters, hclust)], id.vars = c('slope', 'n_clusters', 'hclust')) 
to_plot <- to_plot[value > 0]
to_plot$hclust<- factor(to_plot$hclust)
levels(to_plot$hclust) <- c("CEU", "MED", "NEU")

g3 <- ggplot(to_plot[slope == 'neg' & n_clusters > 1000], aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.colour = NA) + 
  scale_y_continuous(limits = c(0, 3)) +
  geom_hline(yintercept = 1, linetype = 2, col = 'grey30') +
  facet_wrap(~hclust) +
  scale_fill_manual(values = c(var_cols[1:3])) +
  theme_bw() +
  theme_opts +
  labs(x = 'Variable', y = 'Mean def. volume') +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80'))  

g4 <- ggplot(to_plot[slope == 'pos' & n_clusters > 1000], aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.colour = NA) + 
  scale_y_continuous(limits = c(0, 3)) +
  geom_hline(yintercept = 1, linetype = 2, col = 'grey30') +
  facet_wrap(~hclust) +
  scale_fill_manual(values = c(var_cols[1:3])) +
  theme_bw() +
  theme_opts +
  labs(x = 'Variable', y = 'Mean def. volume') +
  theme(strip.background = element_rect(fill = 'grey30', colour = 'grey30')) +
  theme(strip.text = element_text(colour = 'grey80'))  

gg_all <- ggarrange(g3, g4,  labels = c("a", "b"), common.legend = T, legend = 'right', nrow = 2, ncol = 1)
ggsave('./results/figs/flash_dr_intensity.png', plot = gg_all, height = 6, width = 5)

gg_all <- ggarrange(g2, g3, g1, g4, 
                    labels = c("a", "c", "b", "d"), 
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 2)
ggsave('./results/figs/flash_dr.png', plot = gg_all, height = 5, width = 8)

#Flash droughts
to_plot <- timeseries_all[dur_cat == '3-6', .N, .(month = month(DTM), 
                                                  cluster = period, 
                                                  variable = factor(variable),
                                                  region = REG)]
to_plot[variable == 'p_dv', N := as.integer(mean(N)), .(cluster, region)]
to_plot[variable == 'q_dv', N := as.integer(mean(N)), .(cluster, region)]
ggplot(to_plot, aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(region~cluster) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Grid cells') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('./results/figs/flash_dr_change.png', height = 8, width = 10)





