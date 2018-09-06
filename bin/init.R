#First script to run

#Folder structure
 dir.create("./data"); dir.create("./results"); dir.create("./results/figs") #results and data folders are ignored due to size (.gitignore)

source('./source/functions.R') 
 
#Data preparation
dta <- readRDS('./data/raw/mstat_rQ20_rs_len0_met_001.Rds') #main analysis dataset


dta[, yr := year(DTM)]
dta[, month := month(DTM)]
dta[, PT_ID := .GRP, .(x, y)] #id for each grid cell

#events begin when !is.na(p3 | s | q) 
table(dta[EVE == T]$EID) #EID is not unique for each event - serial number of event at each grid cell (below q20) EID
dta[EVE == T & EID > 0, ID := .GRP, .(x, y, EID)] #in this way we have event IDs for each grid box
dta[EVE == T, start := min(DTM, na.rm = T), ID] 
dta[EVE == T, start_month := month(min(DTM, na.rm = T)), ID] 
dta[(start_month >= 1 & start_month <= 2) | start_month >= 9, type := factor('cold')]
dta[(start_month >= 3 & start_month <= 8), type := factor('warm')]
dta[!is.na(ID), dur := .N, ID] #and their duration in months 

dta[!is.na(s), start_s := min(DTM, na.rm = T), ID]
dta[!is.na(q), start_q := min(DTM, na.rm = T), ID]
setorder(dta, PT_ID)
saveRDS(dta, './data/mstat_all_met1.Rds') #dataset used in further analysis

events <- dta[!is.na(ID) & !is.na(REG), .(REG, PT_ID, x, y, ID, type, start, start_month, dur, start_s, start_q)]
events[, start_s := max(start_s, na.rm = T), .(PT_ID, ID)]
events[, start_q := max(start_q, na.rm = T), .(PT_ID, ID)]
events <- events[!duplicated(events)]
events[, start_diff := unique(month(start_s)) - unique(month(start_q)), ID]


saveRDS(events, './data/events_met1.Rds') #event information


#dta_nvars <- dta[, .(EVE, REG, DTM, yr, month, time, PT_ID, x, y, ID, start, start_month, dur, nP, nP3, nQ, nS, nT, nPET)] 
#saveRDS(dta_nvars, './data/mstat_nvars_met1.Rds') #dataset with normalised values [to work with a smaller data table]
#rm(dta, dta_nvars)

## Examples

#Events happened at grid cell (34, 24) :
test <- dta[x == 34 & y == 24]
test[, table(ID)]

#The top 30 droughts in terms of duration at this grid cell are:
test_top_30_ids <- head(unique(test[order(-dur), .(ID, dur)]), 30)
test_top_30 <- dta[ID %in% test_top_30_ids$ID]


