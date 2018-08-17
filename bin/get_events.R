#Detect and classify the events, i.e., flash droughts

source('./source/libs.R'); source('./source/graphics.R')
dta <- readRDS('./data/mstat.Rds')

dta_CEU <- dta[REG == 'CEU']
dta_MED <- dta[REG == 'MED']

#The 100 most persisting droughts in all grid cells per region
top_100_ids_CEU <- head(unique(dta_CEU[order(-dur), .(ID, dur)]), 100)
top_100_ids_MED <- head(unique(dta_MED[order(-dur), .(ID, dur)]), 100)
top_100_CEU <- dta[ID %in% top_100_ids_CEU$ID]
top_100_MED <- dta[ID %in% top_100_ids_MED$ID]

#The vegetation period droughts, JJA (strictly 3 months of drought during summer)
veg_ids_start <- dta_eve[month == 6, ID]
veg_ids_end <- dta_eve[month == 8, ID]
veg_dr <- dta_eve[ID %in% veg_ids_start & ID %in% veg_ids_end & dur == 3]
saveRDS(veg_dr, file = './data/veg_droughts_3_strict.Rds')

#Again strictly 3 months of drought during summer, but with total duration 5 months, i.e., drought continues before or after summer
veg_dr <- dta_eve[ID %in% veg_ids_start & ID %in% veg_ids_end & dur == 5]
saveRDS(veg_dr, file = './data/veg_droughts_5_strict.Rds')

#Again strictly 3 months of drought during summer, but not conditional to duration, i.e., all droughts that propagated in summer
veg_dr <- dta_eve[ID %in% veg_ids_start & ID %in% veg_ids_end]
saveRDS(veg_dr, file = './data/veg_droughts_all_strict.Rds')

#Similar to 2003 drought
ggplot(dta_eve[REG == 'CEU' & yr == 2003, .(dur)], aes(x = dur)) +
  geom_histogram(col = 'black', fill = var_cols[2]) +
  xlab('duration (months)') +
  theme_bw() +
  theme_opts

ggplot(dta_eve[REG == 'CEU' & yr == 2003, .(month)], aes(x = factor(month), stat = 'count')) +
  stat_count(col = 'black', fill = var_cols[4]) +
  xlab('months') +
  theme_bw() +
  theme_opts

veg_ids_start <- dta_eve[month == 3, ID]
veg_ids_end <- dta_eve[month == 9, ID]
veg_dr <- dta_eve[ID %in% veg_ids_start & ID %in% veg_ids_end & dur == 7]
saveRDS(veg_dr, file = './data/veg_droughts_2003_strict.Rds')



