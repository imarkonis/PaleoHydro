#Combine x, y with lat,lon and make some plots to get an idea where the PT_IDs are located in the map

source('./source/functions.R'); 
dta_sp <- readRDS('./data/spatial.Rds')
dta_sp_rnd <- dta_sp[runif(100, 1, max(dta_sp$PT_ID)), ]
plot(dta_sp_rnd$x, dta_sp_rnd$y, col = 'white') 
text(dta_sp_rnd$x, y = dta_sp_rnd$y, labels = dta_sp_rnd$PT_ID)
