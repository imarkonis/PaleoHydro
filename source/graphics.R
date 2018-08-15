source('./source/libs.R')

theme_opts <- list(theme(axis.ticks.length=unit(-0.1, "cm"),  
                         axis.text.x = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
                         axis.text.y = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm"))))

var_cols <- c("#32384D", "#D13525",  "#F2C057",  "#217CA3","#426E86")

veg_spa_time <- unique(veg_dr[, .(PT_ID, x, y, yr)]) #not taking month in order to merge the whole year
veg_dr <- merge(veg_spa_time, dta)
veg_dr[, month := month(DTM)]

veg_dr_CEU <- veg_dr[REG == 'CEU', .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)] #nPET has a very sharp density
plot(melt(table(veg_dr_CEU[, yr])), type = 'l')
dt_ceu <- melt(data = veg_dr_CEU, id.vars = c('PT_ID', 'yr', 'month')) 
dt_ceu[is.na(value), value := 0]

plot_var_dens <- function(dt){
  ggplot(dt, aes(x = value, fill = variable, group = variable)) +
  geom_density(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.3, col = var_cols[1]) + 
  xlim(-3, 3) +
  scale_fill_manual(values = var_cols[c(1, 5, 4, 3, 2)]) +
  facet_wrap(vars(month)) +
  theme_bw() + 
  theme(strip.background = element_rect(fill = var_cols[1])) +
  theme(strip.text = element_text(colour = 'white')) + 
  theme_opts
}
