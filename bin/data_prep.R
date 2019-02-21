#Data_preparation: creates and changes variable names, as well as data tables for analysis

#Uses: mstat_all_met1.Rds, stat_all_met1.Rds, rs_met_001.rds

#Creates: event_ids_met1.Rds, events_met1.Rds, timeseries_met1.Rds
#         mstat_short_met1.Rds, spatial.Rds
#         dataset_met1.Rdata

#Updates: mstat_all_met1.Rds


source('./source/functions.R') 

dtm <- readRDS('./data/mstat_all_met1.Rds') 
dts <- readRDS('./data/stat_all_met1.Rds') 

#Create IDs
dtm[, PT_ID := .GRP, .(x, y)] #grid point IDs
dtm[EVE == T & EID > 0, ID := .GRP, .(x, y, EID)] #drought event IDs 
setorder(dtm, PT_ID, DTM, ID)
dtm[, p_ID := .GRP * !is.na(p3) , rleid(!is.na(p3))] #precip drought IDs
dtm[, q_ID := .GRP * !is.na(q) , rleid(!is.na(q))] #runoff drought IDs
dtm[, s_ID := .GRP * !is.na(s) , rleid(!is.na(s))] #soil moisture drought IDs
dtm[p_ID == 0, p_ID := NA]
dtm[q_ID == 0, q_ID := NA]
dtm[s_ID == 0, s_ID := NA]
event_ids <- unique(dtm[, .(PT_ID, ID, p_ID, q_ID, s_ID)])
event_ids <- event_ids[!is.na(ID)]
setorder(event_ids, PT_ID, ID)
setkey(event_ids, ID)
saveRDS(event_ids, './data/event_ids_met1.Rds') #links between different type of drought events
#event_ids <- readRDS('./data/event_ids_met1.Rds') 

#Time series
timeseries <- dtm[!is.na(REG), .(REG, PT_ID, ID, DTM,
                                 p = aP, p3 = aP3, q = aQ, s = aS, pet = aPET, t = aT,
                                 p_dv = p, p3_dV = p3, q_dv = q, s_dv = s, pet_ev = u_pet, t_ev = u_t)] 
saveRDS(timeseries, './data/timeseries_met1.Rds')
#timeseries <- readRDS('./data/timeseries_met1.Rds')

#Event properties
dtm[EVE == T, start := min(DTM, na.rm = T), ID] 
dtm[!is.na(ID), dur := .N, ID] #and their duration in months 
dtm[EVE == T, end := max(DTM, na.rm = T), ID] 
dtm[!is.na(p3), start_p3 := min(DTM, na.rm = T), ID]
dtm[!is.na(q), start_q := min(DTM, na.rm = T), ID]
dtm[!is.na(s), start_s := min(DTM, na.rm = T), ID]
dtm[!is.na(p_ID), p3_dur := .N, p_ID] 
dtm[is.na(p3_dur), p3_dur := 0]
dtm[!is.na(q_ID), q_dur := .N, q_ID] 
dtm[!is.na(s_ID), s_dur := .N, s_ID] 
setorder(dtm, PT_ID, DTM, ID)
saveRDS(dtm, './data/mstat_all_met1.Rds') #dataset with all variables with OLD NAMES!

#Determine events
events <- unique(dtm[!is.na(ID) & !is.na(REG), .(REG, PT_ID, ID, p_ID, q_ID, s_ID, 
                                                 start, dur, end, 
                                                 start_p3, start_q, start_s,
                                                 p3_dur, q_dur, s_dur,
                                                 p_dv_m = mean(p, na.rm = T), 
                                                 q_dv_m = mean(q, na.rm = T), 
                                                 s_dv_m = mean(s, na.rm = T), 
                                                 pet_ev_m = mean(u_pet, na.rm = T), 
                                                 t_ev_m = mean(u_t, na.rm = T)), ID])

events[, start_p3 := max(start_p3, na.rm = T), ID]
events[, start_q := max(start_q, na.rm = T), ID]
events[, start_s := max(start_s, na.rm = T), ID]

aa <- unique(events[, .(ID, p3_dur), p_ID])   #The total duration of each drought type during the drougth event
aa <- aa[, .(p3_dur_tot = sum(p3_dur)), ID]
events <- events[aa, on = 'ID']

aa_1 <- unique(events[ID < 35000, .(ID, q_dur), q_ID])  #some bug that cannot find or explain
aa_2 <- unique(events[ID > 35000 & ID <= 55000, .(ID, q_dur), q_ID])
aa_3 <- unique(events[ID > 55000, .(ID, q_dur), q_ID])
aa <- rbind(aa_1, aa_2, aa_3)
aa <- aa[, .(q_dur_tot = sum(q_dur)), ID]
events <- events[aa, on = 'ID']

aa <- unique(events[, .(ID, s_dur), s_ID])
aa <- aa[, .(s_dur_tot = sum(s_dur)), ID]
events <- events[aa, on = 'ID']

events[year(start) >= 1900, period := cut(year(start), 4,  c('1900-1929', '1930-1959', '1960-1988', '1989-2018'))]
events[, dur_cat := cut(dur, c(0, 2, 6, 12, 100), c('0-2', '3-6', '7-12', '>12'))]
events[, ID.1 := NULL]
saveRDS(events, './data/events_met1.Rds') #event information
#events <- readRDS('./data/events_met1.Rds') 

dtm_short <- dtm[, .(REG, DTM, PT_ID, ID, p_ID, q_ID, s_ID, start, end, dur, start_s, start_q, start_p3,
                     p = aP, p3 = aP3, q = aQ, s = aS, t = aT, pet = aPET,
                     p_dv = p, p3_dv = p3, q_dv = q, s_dv = s, t_ev = u_t, pet_ev = u_pet)] 
saveRDS(dtm_short, './data/mstat_short_met1.Rds') #short version of dtm [for efficiency] with NEW NAMES!

dtb <- readRDS('./data/raw/rs_met_001.rds')
dts_sp <- unique(dts[, 1:4])
dtm_sp <- unique(dtm[, .(PT_ID, x, y)])
dtm_sp <- merge(dts_sp, dtm_sp, by = c('x', 'y'))
coords <- dtm_sp[, c(5, 1:4)]
setnames(coords, c('LON', 'LAT'), c('lon', 'lat'))
setorder(coords, PT_ID)
setkey(coords, PT_ID)
saveRDS(coords, file = './data/spatial.Rds') #point IDS, lat/lon and x/y coords
#coords <- readRDS('./data/spatial.Rds')

save(coords, events, event_ids, timeseries, file = './data/dataset_met1.Rdata') #All data needed for analyses



