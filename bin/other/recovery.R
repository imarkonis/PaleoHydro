#Explore the recovery after drought events

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_nvars.Rds')
veg_dr <- readRDS(file = './data/veg_droughts_AMJ_5_8.Rds')

months_after <- 6

#The 2003 event
veg_dr_2003 <- veg_dr[yr == 2003] 
veg_dr_2003 <- add_nxt_yr(veg_dr_2003)
abs_end <- unique(veg_dr_2003[month == 9 & event == 'cur_yr', time])

veg_dr_2003_nxt <- veg_dr_2003[time > abs_end & time <= (abs_end + months_after),
                       .(PT_ID, time, nP, nP3, nQ, nS, nT)]

to_plot <- melt(data = veg_dr_2003_nxt, id.vars = c('PT_ID', 'time'))
to_plot[, time := time - abs_end]
plot_var_dens_aft(to_plot)