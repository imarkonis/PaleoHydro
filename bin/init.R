#First script to run

dir.create("./data"); dir.create("./source"); dir.create("./bin"); dir.create("./results"); dir.create("./results/figs")

#Data preparation
dta <- readRDS('./data/mstat_rQ20_rs_len0_rs_1.Rds') #main analysis dataset

dta[, month := month(DTM)]
dta[, abs_month := .GRP, DTM]
dta[, PT_ID := .GRP, .(x, y)] #point ids

table(dta_eve[EVE == T]$yr) # EID seems not to be specific for each grid box; but they are continuous in time
dta[, ID := .GRP, .(x, y, EID)] # In this way we have event IDs for each grid box
dta[, dur := .N, ID] # and their duration in months

saveRDS(dta, './data/mstat.Rds')

#For example, the following events happened at grid cell (34, 24) :
test <- dta[x == 34 & y == 24]
test[, table(ID)]

#The top 30 droughts in terms of duration at this grid cell are:
test_top_30_ids <- head(unique(test[order(-dur), .(ID, dur)]), 30)
test_top_30 <- dta_eve[ID %in% test_top_30_ids$ID]


