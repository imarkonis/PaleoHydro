#First script to run

dir.create("./data"); dir.create("./source"); dir.create("./bin"); dir.create("./results"); dir.create("./results/figs")

#Data preparation
dta <- readRDS('./data/mstat_rQ20_rs_len0_rs_1.Rds') #main analysis dataset

dta[, month := month(DTM)]
dta[, abs_month := .GRP, DTM]
dta[, PT_ID := .GRP, .(x, y)] #point ids

saveRDS(dta, './data/mstat.Rds')
