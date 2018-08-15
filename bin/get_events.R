#Detect and classify the events, i.e., flash droughts

source('./source/libs.R')
dta <- saveRDS('./data/mstat.Rds')

dta_eve <- dta[EVE == T]; rm(dta); gc()

table(dta_eve$yr) # EID seems not to be specific for each grid box; but they are continuous in time
dta_eve[, ID := .GRP, .(x, y, EID)] # In this way we have event IDs for each grid box
dta_eve[, dur := .N, ID] # and their duration in months
dta_eve_CEU <- dta_eve[REG == 'CEU']
dta_eve_MED <- dta_eve[REG == 'MED']

#For example, the following events happened at grid cell (34, 24) :
test <- dta_eve[x == 34 & y == 24]
test[, table(ID)]

#The top 30 droughts in terms of duration in this point are:
test_top_30_ids <- head(unique(test[order(-dur), .(ID, dur)]), 30)
test_top_30 <- dta_eve[ID %in% test_top_30_ids$ID]

#The 100 most persisting droughts in all grid cells per region
top_100_ids_CEU <- head(unique(dta_eve_CEU[order(-dur), .(ID, dur)]), 100)
top_100_ids_MED <- head(unique(dta_eve_MED[order(-dur), .(ID, dur)]), 100)
top_100_CEU <- dta_eve[ID %in% top_100_ids_CEU$ID]
top_100_MED <- dta_eve[ID %in% top_100_ids_MED$ID]

#The vegetation period droughts, JJA (strictly 3 months of drought during summer)
veg_ids_start <- dta_eve[month == 6, ID]
veg_ids_end <- dta_eve[month == 8, ID]
veg_dr <- dta_eve[ID %in% veg_ids_start & ID %in% veg_ids_end & dur == 3]
saveRDS(veg_dr, file = './data/veg_droughts_3_strict.Rds')

#Again strictly 3 months of drought during summer, but with total duration 5 months, i.e. drought continues before or after summer
veg_dr <- dta_eve[ID %in% veg_ids_start & ID %in% veg_ids_end & dur == 5]
saveRDS(veg_dr, file = './data/veg_droughts_5_strict.Rds')
