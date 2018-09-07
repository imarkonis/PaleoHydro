source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_all_met1.Rds')
dtb <- readRDS('./data/raw/rs_met_001.rds')

colnames(dtb)[1:2] = c('x', 'y')
dtb_sp <- unique(dtb[, 1:4])
dta_sp <- unique(dta[, .(x, y, PT_ID)])
dta_sp <- merge(dta_sp, dtb_sp, by = c('x', 'y'))

saveRDS(dta_sp, file = './data/spatial.Rds')

dta_sp_rnd <- dta_sp[runif(100, 1, max(dta_sp$PT_ID)), ]
plot(dta_sp_rnd$x, dta_sp_rnd$y, col = 'white') 
text(dta_sp_rnd$x, y = dta_sp_rnd$y, labels = dta_sp_rnd$PT_ID)
