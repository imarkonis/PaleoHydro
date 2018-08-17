#Explore the preconditions of drought events

source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_nvars.Rds')
veg_dr <- readRDS(file = './data/veg_droughts_AMJ_5_8.Rds')

months_before <- 6
veg_dr_2003 <- veg_dr[yr == 2003, .(PT_ID, yr, month, event, time, nP, nP3, nQ, nS, nT)] 
veg_dr_2003 <- add_prv_yr(veg_dr_2003)

abs_start <- unique(veg_dr_2003[month == 3 & event == 'cur_yr', time])
veg_dr_2003_prv <- veg_dr_2003[time < abs_start & time >= (abs_start - months_before),
                       .(PT_ID, time, nP, nP3, nQ, nS, nT)]

to_plot <- melt(data = veg_dr_2003_prv, id.vars = c('PT_ID', 'time'))
to_plot[, time := time - abs_start]
plot_var_dens_pre(to_plot)




