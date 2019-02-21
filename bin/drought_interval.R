source('./source/functions.R'); source('./source/graphics.R'); 

events_som <- readRDS('./data/events_som.Rds')
setorder(events_som, PT_ID, start)
events_som[, next_start := shift(start, 1, type = 'lead'), PT_ID]
events_som[, dr_interval := as.numeric(round((next_start - (start + months(dur)))/30)), PT_ID]

aa <- events_som[, .(dr_interval), .(slope)]
ggplot(aa, aes(x = dr_interval, fill = slope)) +
  geom_density(alpha = 0.8) +
  coord_cartesian(xlim = c(0, 24)) +
  scale_fill_manual(values = c(var_cols[c(1, 4)])) +
  scale_x_continuous(limits = c(0, 60)) +
  labs(x = 'Drought interval (months)', y = 'Density') +
  theme_bw()
ggsave('./results/figs/som/dr_intervals.png', height = 4, width = 3)



events_som[n_clusters > 1000 & dur < 7 & hclust %in% c('CEU_1', 'MED_7', 'NEU_3'), .(dur_m = mean(dur), 
                                           dr_interval_m = mean(dr_interval, na.rm = T),
                                           .N), .(period)]


