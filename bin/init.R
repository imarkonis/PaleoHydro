#First script to run

#Folder structure
 dir.create("./data"); dir.create("./results"); dir.create("./results/figs") #results and data folders are ignored due to size (.gitignore)

source('./source/functions.R') 
 
#Data preparation
dta <- readRDS('./data/mstat_rQ20_rs_len0_rs_1.Rds') #main analysis dataset

dta[, yr := yr]
dta[, month := month(DTM)]
dta[, time := .GRP, DTM] #absolute time units
dta[, PT_ID := .GRP, .(x, y)] #id for each grid cell

table(dta[EVE == T]$yr) #EID seems not to be specific for each grid box; but they are continuous in time
dta[EVE == T, ID := .GRP, .(x, y, EID)] #in this way we have event IDs for each grid box
dta[EVE == T, start := min(time, na.rm = T), ID] 
dta[EVE == T, start_month := month(min(DTM, na.rm = T)), ID] 
dta[!is.na(ID), dur := .N, ID] #and their duration in months 
setorder(dta, PT_ID)

saveRDS(dta, './data/mstat_all.Rds') #dataset used in further analysis

dta_nvars <- dta[, .(EVE, REG, DTM, yr, month, time, PT_ID, x, y, ID, start, start_month, dur, nP, nP3, nQ, nS, nT, nPET)] 
saveRDS(dta_nvars, './data/mstat_nvars.Rds') #dataset with normalised values [to work with a smaller data table]
rm(dta, dta_nvars)

## Examples

#Events happened at grid cell (34, 24) :
test <- dta[x == 34 & y == 24]
test[, table(ID)]

#The top 30 droughts in terms of duration at this grid cell are:
test_top_30_ids <- head(unique(test[order(-dur), .(ID, dur)]), 30)
test_top_30 <- dta[ID %in% test_top_30_ids$ID]


