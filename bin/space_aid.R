source('./source/functions.R'); source('./source/graphics.R') 
dta <- readRDS('./data/mstat_all_met1.Rds')

dta_sp <- unique(dta[, .(x, y, PT_ID)])
dta_sp_rnd <- dta_sp[runif(100, 1, max(dta_sp$PT_ID)), ]
plot(dta_sp_rnd$x, dta_sp_rnd$y, col = 'white') 
text(dta_sp_rnd$x, y = dta_sp_rnd$y, labels = dta_sp_rnd$PT_ID)

#lat, lon in rs_met_001.rds