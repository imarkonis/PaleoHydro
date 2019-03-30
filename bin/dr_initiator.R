source('./source/functions.R'); source('./source/graphics.R'); 

events_som <- readRDS('./data/events_som.Rds')
events <- readRDS('./data/events_met1.Rds')
dta <- readRDS('./data/timeseries_met1.Rds')
events_start <- events[, .SD[1], ID]
aa <- events_start[, .(ID, start_pet)]   #pet_start was not estimated in first data prep so we have to merge it here
events_som <- aa[events_som, on = 'ID']
events_som[, start_pet_month := month(start_pet)]
events_som[start_p3_month == 0 , start_p3_month := NA]

events_som[is.na(start_p3_month)]
events_som[is.na(start_pet_month), table(hclust)]

events_som[!is.na(start_pet_month) & !is.na(start_p3_month) & hclust == 'CEU_1', 
           mean(start_p3 - start_pet), by = .(cluster, slope, n_clusters)]

init <- 
  
  
length(init[init>0]) / length(init)
length(init[init<0]) / length(init)
length(init[init==0]) / length(init)
events_som[ID == 213, start_p3 - start_pet ]
events_som[ID == 213]
