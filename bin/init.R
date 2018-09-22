#First script to run

#Folder structure
 dir.create("./data");  dir.create("./data/raw"); dir.create("./results"); #results and data folders are ignored due to size (.gitignore)
 dir.create("./results/figs"); dir.create("./results/figs/2018"); dir.create("./results/figs/distributions")
source('./source/functions.R') 
 
#Data preparation
dts <- readRDS('./data/raw/stat_rQ20_rs_len0_met_001.Rds') #main analysis dataset
dtm <- readRDS('./data/raw/mstat_rQ20_rs_len0_met_001.Rds') #main analysis dataset

e = nc_open('./data/raw/eobs_mask_total2D.nc')
xc = ncvar_get(e, 'xc')
yc = ncvar_get(e, 'yc')
msk = ncvar_get(e, 'mask_pre')
dimnames(msk) = list(xc = xc, yc = yc)
msk = data.table(melt(msk))
setnames(msk, 'value', 'INCLUDE')

dts <- msk[dts, on = c('xc' = 'x', 'yc' = 'y')]
dtm <- msk[dtm, on = c('xc' = 'x', 'yc' = 'y')]

dts <- dts[INCLUDE == 1]; dts[, INCLUDE := NULL]
dtm <- dtm[INCLUDE == 1]; dtm[, INCLUDE := NULL]

setnames(dts, c('xc', 'yc'), c('x', 'y'))
setnames(dtm, c('xc', 'yc'), c('x', 'y'))

dtm[, yr := year(DTM)]
dtm[, month := month(DTM)]
dtm[, PT_ID := .GRP, .(x, y)] #id for each grid cell

#events begin when !is.na(p3 | s | q) 
# table(dtm[EVE == T]$EID) #EID is not unique for each event - serial number of event at each grid cell (below q20) EID
dtm[EVE == T & EID > 0, ID := .GRP, .(x, y, EID)] #in this way we have event IDs for each grid box
dtm[y > 3500000, REG := 'NEU'] #fix for minor discrepancy in labeling
dtm[EVE == T, start := min(DTM, na.rm = T), ID] 
dtm[EVE == T, start_month := month(min(DTM, na.rm = T)), ID] 
dtm[(start_month >= 1 & start_month <= 2) | start_month >= 9, type := factor('cold')]
dtm[(start_month >= 3 & start_month <= 8), type := factor('warm')]
dtm[!is.na(ID), dur := .N, ID] #and their duration in months 

dtm[!is.na(s), start_s := min(DTM, na.rm = T), ID]
dtm[!is.na(q), start_q := min(DTM, na.rm = T), ID]
dtm[!is.na(p3), start_p3 := min(DTM, na.rm = T), ID]
setorder(dtm, PT_ID)
#saveRDS(dtm, './data/mstat_all_met1.Rds') #dataset with all variables that can be used in further analysis

dtb <- readRDS('./data/raw/rs_met_001.rds')
dts_sp <- unique(dts[, 1:4])
dtm_sp <- unique(dtm[, .(x, y, PT_ID)])
dtm_sp <- merge(dtm_sp, dts_sp, by = c('x', 'y'))

saveRDS(dtm_sp, file = './data/spatial.Rds') #point IDS, lat/lon and x/y coords

events <- dtm[!is.na(ID) & !is.na(REG), .(REG, PT_ID, x, y, ID, type, start, start_month, dur, start_s, start_q, start_p3, 
                                          p_dv = p, q_dv = q, s_dv = s, pet_ev = u_pet, t_ev = u_t)] 
events[, start_q := max(start_q, na.rm = T), .(PT_ID, ID)]
events[, start_p3 := max(start_p3, na.rm = T), .(PT_ID, ID)]
events[, start_s := max(start_s, na.rm = T), .(PT_ID, ID)]
events[, p_dv_m := mean(p_dv, na.rm = T), ID]
events[, q_dv_m := mean(q_dv, na.rm = T), ID]
events[, s_dv_m := mean(s_dv, na.rm = T), ID]
events[, pet_ev_m := mean(pet_ev, na.rm = T), ID]
events[, t_ev_m := mean(t_ev, na.rm = T), ID]
events[, p_dv := NULL]      
events[, q_dv := NULL]      
events[, s_dv := NULL]      
events[, t_ev := NULL]      
events[, pet_ev := NULL]      
events <- events[!duplicated(events)]
events[is.na(p_dv_m), p_dv_m := 0]
events[is.na(q_dv_m), q_dv_m := 0]
events[is.na(s_dv_m), s_dv_m := 0]
events[is.na(pet_ev_m), pet_ev_m := 0]
events[is.na(t_ev_m), t_ev_m := 0]
#events[, start_diff := unique(month(start_s)) - unique(month(start_q)), ID]

saveRDS(events, './data/events_met1.Rds') #event information

dtm_short <- dtm[, .(REG, DTM, PT_ID, ID, start, start_month, dur, type, start_s, start_q, start_p3,
                     p = aP, p3 = aP3, q = aQ, s = aS, t = aT, pet = aPET,
                     p_dv = p, p3_dv = p3, q_dv = q, s_dv = s, t_ev = u_t, pet_ev = u_pet)] 
saveRDS(dtm_short, './data/mstat_short_met1.Rds') #short version of dtm [for efficiency]
rm(dtm, dtm_short)