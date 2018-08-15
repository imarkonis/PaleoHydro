#Explore the preconditions of certain drought events

source('./source/libs.R')

dta <- readRDS('./data/mstat_rQ20_rs_len0_rs_1.Rds')

veg_dr <- readRDS(veg_droughts, file = './data/veg_droughts_3_strict.Rds')

veg_spa_time <- unique(veg_dr[, .(PT_ID, x, y, yr)]) #not taking month in order to merge the whole year
veg_dr <- merge(veg_spa_time, dta)
veg_dr[, month := month(DTM)]

veg_dr_CEU <- veg_dr[REG == 'CEU', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)] #nPET has a very sharp density
plot(table(veg_dr_CEU$yr))
dt_ceu <- melt(data = veg_dr_CEU, id.vars = c('PT_ID', 'yr', 'month')) 
dt_ceu[is.na(value), value := 0]

ggplot(dt_ceu, aes(x = value, fill = variable, group = variable)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.3, col = var_cols[1]) + 
  xlim(-3, 3) +
  scale_fill_manual(values = var_cols[c(1, 5, 4, 3, 2)]) +
  facet_wrap(vars(month)) +
  theme_bw() + 
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('results/figs/veg_dr_3_strict_CEU.tiff', height = 18, width = 18, units = "cm")


veg_dr <- readRDS(veg_droughts, file = './data/veg_droughts_5_strict.Rds')

veg_spa_time <- unique(veg_dr[, .(PT_ID, x, y, yr)]) #not taking month in order to merge the whole year
veg_dr <- merge(veg_spa_time, dta)
veg_dr[, month := month(DTM)]

veg_dr_CEU <- veg_dr[REG == 'CEU', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)] #nPET has a very sharp density
plot(table(veg_dr_CEU$yr))
dt_ceu <- melt(data = veg_dr_CEU, id.vars = c('PT_ID', 'yr', 'month')) 
dt_ceu[is.na(value), value := 0]

ggplot(dt_ceu, aes(x = value, fill = variable, group = variable)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.3, col = var_cols[1]) + 
  xlim(-3, 3) +
  scale_fill_manual(values = var_cols[c(1, 5, 4, 3, 2)]) +
  facet_wrap(vars(month)) +
  theme_bw() + 
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
ggsave('results/figs/veg_dr_5_strict_CEU.tiff', height = 18, width = 18, units = "cm")

# MED
veg_dr_MED <- veg_dr[REG == 'MED', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)]
plot(table(veg_dr_MED$yr))
dt_med <- melt(data = veg_dr_MED, id.vars = c('PT_ID', 'yr', 'month')) 
dt_med[is.na(value), value := 0]

ggplot(dt_med, aes(x = value, fill = variable, group = variable)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.3, col = var_cols[1]) + 
  xlim(-3, 3) +
  scale_fill_manual(values = var_cols[c(1, 5, 4, 3, 2)]) +
  facet_wrap(vars(month)) +
  theme_bw() + 
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts


