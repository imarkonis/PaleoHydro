#Detect and classify the events (within a single year)

source('./source/functions.R'); source('./source/graphics.R')
dta <- readRDS('./data/mstat_short_met1.Rds')
dta_CEU <- dta[REG == 'CEU']
dta_CEU[, yr := year(DTM)]

#The vegetation period droughts, JJA (strictly 3 months of drought during summer)
veg_dr <- dta_CEU[month(start) == 6 & dur == 3]

veg_dr_yr <- merge(unique(veg_dr[, .(PT_ID, yr)]), dta) #adding the rest of the year for each event
#saveRDS(veg_dr, file = './data/veg_droughts_3_strict.Rds')

to_plot <- unique(veg_dr[, .(PT_ID, yr)]) #investigation of temporal evolution
to_plot[, area := .N, yr]  
plot_events_time(to_plot)

to_plot <- melt(data = veg_dr_yr[, .(PT_ID, DTM, yr, month, nP, nP3, nQ, nS, nT)], #nPET has a very sharp density and was not looking good in denstity plots
                id.vars = c('PT_ID', 'yr', 'month', 'DTM')) 
plot_var_dens_yr(to_plot) #looks realistic
ggsave('results/figs/distributions/veg_dr_3_strict_CEU.png', height = 18, width = 18, units = "cm")

#Checking the 2003 drought
to_plot <- melt(data = dta_CEU[yr == 2003, .(PT_ID, DTM, yr, month, nP, nP3, nQ, nS, nT)], 
                id.vars = c('PT_ID', 'yr', 'month', 'DTM')) 
plot_var_dens_yr(to_plot)

ggplot(dta_CEU[yr == 2003], aes(x = dur)) +
  geom_bar(col = 'black', fill = var_cols[2]) +
  xlab('duration (months)') +
  theme_bw() +
  theme_opts

ggplot(dta_CEU[yr == 2003 & EVE == T, .(month)], aes(x = factor(month), stat = 'count')) +
  stat_count(col = 'black', fill = var_cols[4]) +
  xlab('months') +
  theme_bw() +
  theme_opts

#The vegetation period droughts, JJA (strictly 8 months of drought during summer)
veg_ids_start <- dta_CEU[month == 6, ID]
veg_ids_end <- dta_CEU[month == 8, ID]
veg_dr <- dta_CEU[ID %in% veg_ids_start & ID %in% veg_ids_end & dur == 8]
veg_dr_yr <- merge(unique(veg_dr[, .(PT_ID, yr)]), dta) #adding the rest of the year for each event
saveRDS(veg_dr, file = './data/veg_droughts_8_strict.Rds')

to_plot <- unique(veg_dr[, .(PT_ID, yr)]) #investigation of temporal evolution
to_plot[, area := .N, yr]  
plot_events_time(to_plot)

to_plot <- melt(data = veg_dr_yr[, .(PT_ID, DTM, yr, month, nP, nP3, nQ, nS, nT)], 
                id.vars = c('PT_ID', 'yr', 'month', 'DTM')) 
plot_var_dens_yr(to_plot) 
ggsave('results/figs/distributions/veg_dr_8_strict_CEU.tiff', height = 18, width = 18, units = "cm")

#Again strictly 3 months of drought during summer, but with total duration 3 to 9 months, i.e., similar to 2013 drought
veg_dr <- dta_CEU[ID %in% veg_ids_start & ID %in% veg_ids_end & dur >= 3 & dur <= 9]
veg_dr_yr <- dta_CEU[PT_ID %in% veg_dr$PT_ID & yr %in% veg_dr$yr] 

to_plot <- unique(veg_dr[, .(PT_ID, yr)]) 
to_plot[, area := .N, yr]  
plot_events_time(to_plot)

to_plot <- melt(data = veg_dr_yr[, .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)], 
                id.vars = c('PT_ID', 'yr', 'month')) 
plot_var_dens_yr(to_plot) #no clear signal

#At least one month of drought in AMJ and total duration 5 to 8 months, i.e., similar to 2013 drought
veg_ids <- dta_CEU[as.numeric(as.character(month)) >= 4 &
                     as.numeric(as.character(month)) <= 6, ID] 

veg_dr <- dta_CEU[ID %in% veg_ids & dur >= 5 & dur <= 8]
veg_dr_yr <- dta_CEU[PT_ID %in% veg_dr$PT_ID & yr %in% veg_dr$yr] 
saveRDS(veg_dr, file = './data/veg_droughts_AMJ_5_8.Rds')

to_plot <- unique(veg_dr[, .(PT_ID, yr)]) 
to_plot[, area := .N, yr]  
plot_events_time(to_plot) #the 2003, 2012 & 2015 droughts apear the most extensive in the last 50 years -- before 1900 substantial shift in variance in fig
 
to_plot <- melt(data = veg_dr_yr[, .(PT_ID, yr, month, nP, nP3, nQ, nS, nT)], 
                id.vars = c('PT_ID', 'yr', 'month')) 
plot_var_dens_yr(to_plot) #again no clear signal, possibly due to large number of grid cells
ggsave('results/figs/distributions/veg_dr_AMJ_5_8_CEU.tiff', height = 18, width = 18, units = "cm")


#The 100 most persisting droughts in all grid cells per region
top_100_ids_CEU <- head(unique(dta_CEU[order(-dur), .(ID, dur)]), 100)
top_100_CEU <- dta_CEU[ID %in% top_100_ids_CEU$ID]



