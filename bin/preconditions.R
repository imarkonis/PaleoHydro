#Explore the preconditions of certain drought events

source('./source/libs.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat.Rds')

## veg_dr_3_strict_CEU
veg_dr <- readRDS(veg_droughts, file = './data/veg_droughts_3_strict.Rds')
veg_spa_time <- unique(veg_dr[, .(PT_ID, x, y, yr)]) #not taking month in order to merge the whole year
veg_dr <- merge(veg_spa_time, dta)
veg_dr[, month := month(DTM)]

veg_dr_ceu <- veg_dr[REG == 'CEU', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)] #nPET has a very sharp density
plot(melt(table(veg_dr_ceu[, yr])), type = 'l')
dt_ceu <- melt(data = veg_dr_ceu, id.vars = c('PT_ID', 'yr', 'month')) 

plot_var_dens(dt_ceu)
ggsave('results/figs/veg_dr_3_strict_CEU.tiff', height = 18, width = 18, units = "cm")

## veg_dr_5_strict_CEU
veg_dr <- readRDS(veg_droughts, file = './data/veg_droughts_5_strict.Rds')
veg_spa_time <- unique(veg_dr[, .(PT_ID, x, y, yr)]) 
veg_dr <- merge(veg_spa_time, dta)
veg_dr[, month := month(DTM)]

veg_dr_ceu <- veg_dr[REG == 'CEU', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)] 
plot(melt(table(veg_dr_ceu[, yr])), type = 'l')
dt_ceu <- melt(data = veg_dr_ceu, id.vars = c('PT_ID', 'yr', 'month')) 

plot_var_dens(dt_ceu)
ggsave('results/figs/veg_dr_5_strict_CEU.tiff', height = 18, width = 18, units = "cm")

## veg_dr_all_strict_CEU
veg_dr <- readRDS(veg_droughts, file = './data/veg_droughts_all_strict.Rds')

veg_spa_time <- unique(veg_dr[, .(PT_ID, x, y, yr)]) 
veg_dr <- merge(veg_spa_time, dta)
veg_dr[, month := month(DTM)]

veg_dr_ceu <- veg_dr[REG == 'CEU', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)] 
plot(melt(table(veg_dr_ceu[, yr])), type = 'l')
dt_ceu <- melt(data = veg_dr_ceu, id.vars = c('PT_ID', 'yr', 'month')) 

plot_var_dens(dt_ceu)
ggsave('results/figs/veg_dr_all_strict_CEU.tiff', height = 18, width = 18, units = "cm")

## veg_dr_2003_strict_CEU
veg_dr <- readRDS(veg_droughts, file = './data/veg_droughts_2003_strict.Rds')

veg_spa_time <- unique(veg_dr[, .(PT_ID, x, y, yr)]) 
veg_dr <- merge(veg_spa_time, dta)
veg_dr[, month := month(DTM)]

veg_dr_CEU <- veg_dr[REG == 'CEU' & event == 'cur_yr', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)] #all droughts
plot(melt(table(veg_dr_CEU[, yr])), type = 'l') 
dt_ceu <- melt(data = veg_dr_CEU, id.vars = c('PT_ID', 'yr', 'month')) 
plot_var_dens(dt_ceu)
ggsave('results/figs/veg_dr_2003_strict_CEU.tiff', height = 18, width = 18, units = "cm")

veg_dr_CEU <- veg_dr[REG == 'CEU' & yr == '2003' & event == 'cur_yr', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)] #the 2003 drought; looks realistic
dt_ceu <- melt(data = veg_dr_CEU, id.vars = c('PT_ID', 'yr', 'month')) 
plot_var_dens(dt_ceu)



veg_dr <- readRDS(veg_droughts, file = './data/veg_droughts_2003_strict.Rds') #trying to take previous and next year of each event

veg_spa_time <- unique(veg_dr[, .(PT_ID, x, y, yr)]) #not taking month in order to merge the whole year
veg_spa_time_pr <- veg_spa_time_aft <- veg_spa_time
veg_spa_time$event = factor('cur_yr')
veg_spa_time_pr$event = factor('pr_yr')
veg_spa_time_pr[, yr := yr - 1] 
veg_spa_time_aft$event = factor('aft_yr')
veg_spa_time_aft[, yr := yr + 1] 
veg_spa_time <- rbind(veg_spa_time, veg_spa_time_pr, veg_spa_time_aft)
veg_dr <- merge(veg_spa_time, dta)
veg_dr[, month := month(DTM)]

veg_dr_2003 <- veg_dr[REG == 'CEU' & yr == 2003, .(PT_ID, yr, month, event, abs_month, nP, nP3, nQ, nS, nT)] 
abs_start <- unique(veg_dr_2003[month == 3 & event == 'cur_yr', abs_month])
abs_end <- unique(veg_dr_2003[month == 9 & event == 'cur_yr', abs_month])

veg_dr_2003_pre <- dta[REG == 'CEU' & abs_month < abs_start & abs_month >= (abs_start - 6),  #this gets all points,  EVE == T  gives different results
                       .(PT_ID, abs_month, nP, nP3, nQ, nS, nT)]


dt_ceu <- melt(data = veg_dr_2003_pre, id.vars = c('PT_ID', 'abs_month')) 
dt_ceu[, abs_month := abs_month - abs_start]

ggplot(dt_ceu, aes(x = value, fill = variable, group = variable)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.3, col = var_cols[1]) + 
  xlim(-3, 3) +
  scale_fill_manual(values = var_cols[c(1, 5, 4, 3, 2)]) +
  facet_wrap(vars(abs_month)) +
  theme_bw() + 
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts

# MED
veg_dr_med <- veg_dr[REG == 'MED', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)]
plot(table(veg_dr_med$yr))
dt_med <- melt(data = veg_dr_med, id.vars = c('PT_ID', 'yr', 'month')) 

plot_var_dens(dt_med)


