source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_short_met1.Rds')
dta_sp <- readRDS('./data/spatial.Rds')
cz_sp <- dta_sp[LAT >= 48 & LAT <= 50 & LON >= 13 & LON <= 17, PT_ID]
sca_sp <- dta_sp[LAT >= 55 & LAT <= 65 & LON >= 5 & LON <= 30, PT_ID]
ger_sp <- dta_sp[LAT >= 47 & LAT <= 55 & LON >= 5 & LON <= 15, PT_ID]

mhm_dv <- dta[, .(REG, PT_ID, DTM, ID, start, dur, p_dv, q_dv, s_dv)]
mhm_dv <- melt(mhm_dv, id.vars = c('REG', 'PT_ID', 'DTM', 'ID', 'start', 'dur'))
rm(dta); gc()

event_1921 <- mhm_dv[year(DTM) == 1908]
event_1922 <- mhm_dv[year(DTM) == 1909]

table(event_1921[, .(start, DTM)]) #Drought Decomposition Matrix
table(event_1922[, .(start, DTM)]) #Drought Decomposition Matrix
