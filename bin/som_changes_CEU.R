source('./source/functions.R'); source('./source/graphics.R') 

timeseries <- readRDS('./data/timeseries_met1.Rds')[, .(PT_ID, DTM, ID, p_dv, q_dv, s_dv, pet_ev)]
events_som <- readRDS('./data/events_som.Rds')
timeseries <- melt(timeseries, id.vars = c('PT_ID', 'DTM', 'ID'))

aa <- unique(events_som[, .(ID, period, cluster, dur_cat, REG)])
timeseries_all <- aa[timeseries, on = 'ID']
timeseries_all <- timeseries_all[complete.cases(timeseries_all)]

ceu_incr <- unique(events_som[REG == 'CEU' & slope == 'pos' & period == '1989-2018', .N, .(cluster, period, slope)])
ceu_decr <- unique(events_som[REG == 'CEU' & slope == 'neg' & period == '1989-2018', .N, .(cluster, period, slope)])

to_plot <- timeseries_all[REG == 'CEU', .N, .(
  month = month(DTM),
  cluster = factor(cluster),
  variable = factor(variable),
  region = REG
)]

ggplot(to_plot[cluster %in% ceu_decr[N > 300, cluster]], aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~cluster, ncol = 4) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Counts') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts


ggsave(paste0('./results/figs/neu_chng_example.png'), height = 3, width = 8)


ggplot(to_plot[cluster %in% c('NEU_25', 'NEU_22', 'NEU_23', 'NEU_24')], aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~cluster) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Counts') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts

ggplot(to_plot[cluster %in% c('NEU_4', 'NEU_5')], aes(x = month, y = N, col = variable)) +
  geom_point() + 
  geom_line() +
  scale_color_manual(values = var_cols[c(1:3, 5)], labels = c('P', "Q", 'SM', 'PET')) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap(~cluster) +
  labs(color = 'Def./Exc. Volume', x = 'Month', y = 'Counts') +
  theme_bw() +
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
