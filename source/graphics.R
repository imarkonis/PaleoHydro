library(RColorBrewer)
source('./source/functions.R')

theme_opts <- list(theme(axis.ticks.length=unit(-0.1, "cm"),  
                         axis.text.x = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm")), 
                         axis.text.y = element_text(margin=unit(c(0.2,0.2,0.2,0.2), "cm"))))

var_cols <- c("#32384D", "#D13525",  "#F2C057",  "#217CA3","#426E86")
colset_mid <- c( "#4D648D", "#337BAE", "#97B8C2",  "#739F3D", "#ACBD78",  
                 "#F4CC70", "#EBB582",  "#BF9A77",
                 "#E38B75", "#CE5A57",  "#D24136", "#785A46" )
colset_mid_qual <- colset_mid[c(11, 2, 4, 6,  1, 8, 10, 5, 7, 3, 9, 12)]
palette_mid <- colorRampPalette(colset_mid)
palette_mid_qual <- colorRampPalette(colset_mid_qual)
palette_spectral <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space = "Lab")


plot.events.time <- function(dt){
  ggplot(dt, aes(x = yr, y = area)) + 
  geom_point(size = 2) + 
  geom_segment(aes(x = yr, 
                   xend = yr, 
                   y = 0, 
                   yend = area)) + 
  xlab('Time (years)') + 
  ylab('Number of grid cells') + 
  theme_bw() +
  theme_opts + 
  scale_y_continuous(expand = c(0, 0))} 

plot.var.dens.yr <- function(dt){
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

plot.var.dens.prv <- function(dt){
  ggplot(dt, aes(x = value, fill = variable, group = variable)) +
    geom_density(alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.3, col = var_cols[1]) + 
    xlim(-3, 3) +
    scale_fill_manual(values = var_cols[c(1, 5, 4, 3, 2)]) +
    facet_wrap(vars(time)) +
    theme_bw() + 
    theme(strip.background = element_rect(fill = var_cols[1])) +
    theme(strip.text = element_text(colour = 'white')) + 
    theme_opts
}

plot.var.dens.nxt <- function(dt){
  dt[, time_f := (paste0('+', as.character(time)))]
  ggplot(dt, aes(x = value, fill = variable, group = variable)) +
    geom_density(alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.3, col = var_cols[1]) + 
    xlim(-3, 3) +
    scale_fill_manual(values = var_cols[c(1, 5, 4, 3, 2)]) +
    facet_wrap(vars(time_f)) +
    theme_bw() + 
    theme(strip.background = element_rect(fill = var_cols[1])) +
    theme(strip.text = element_text(colour = 'white')) + 
    theme_opts
}
