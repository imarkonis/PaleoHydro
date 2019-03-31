source('./source/functions.R'); source('./source/graphics.R'); 

events_som <- readRDS('./data/events_som.Rds')
events <- readRDS('./data/events_met1.Rds')
dta <- readRDS('./data/timeseries_met1.Rds')
events_start <- events[, .SD[1], ID]
aa <- events_start[, .(ID, start_pet)]   #pet_start was not estimated in first data prep so we have to merge it here
events_som <- aa[events_som, on = 'ID']
events_som[, start_pet_month := month(start_pet)]
events_som[start_p3_month == 0 , start_p3_month := NA]

events_som[is.na(start_p3_month), table(hclust)]
events_som[is.na(start_pet_month), table(hclust)]

ceu_fldr_init <- events_som[!is.na(start_pet_month) & !is.na(start_p3_month) & hclust == 'CEU_1' , 
           .(pvt = start_p3 - start_pet, slope)]

ceu_fldr_init[, .(mean_lag = mean(pvt)), slope] 
ceu_fldr_init[pvt < 0, init := 'p']
ceu_fldr_init[pvt > 0, init := 't']
ceu_fldr_init[pvt == 0, init := 's']
ceu_fldr_init[, table(init), slope]

neu_fldr_init <- events_som[!is.na(start_pet_month) & !is.na(start_p3_month) & hclust == 'NEU_3' , 
                            .(pvt = start_p3 - start_pet, slope)]
neu_fldr_init[, .(mean_lag = mean(pvt)), slope] 
neu_fldr_init[pvt < 0, init := 'p']
neu_fldr_init[pvt > 0, init := 't']
neu_fldr_init[pvt == 0, init := 's']
neu_fldr_init[, table(init), slope]

med_fldr_init <- events_som[!is.na(start_pet_month) & !is.na(start_p3_month) & hclust == 'MED_7' , 
                            .(pvt = start_p3 - start_pet, slope)]
med_fldr_init[, .(mean_lag = mean(pvt)), slope] 
med_fldr_init[pvt < 0, init := 'p']
med_fldr_init[pvt > 0, init := 't']
med_fldr_init[pvt == 0, init := 's']
med_fldr_init[, table(init), slope]

fldr_init_yr <- events_som[!is.na(start_pet_month) & !is.na(start_p3_month), 
                               .(pvt = start_p3 - start_pet, slope, year, REG)]

to_plot <- fldr_init_yr[, .(mean_lag = mean(pvt)), .(slope, year, REG)] 
ggplot(to_plot, aes(year, as.numeric(mean_lag), col = REG)) +
#  geom_line() +
  geom_smooth(se = F, span = 0.5) +
  theme_bw()

ceu_fldr_qvs <- events_som[start_q != Inf & start_q != -Inf &
                           start_s != Inf & start_s != -Inf &
                           hclust == 'CEU_1', .(qvs = start_q - start_s, slope)]

ceu_fldr_qvs[, .(mean_lag = mean(qvs, na.rm = T)), slope] 
ceu_fldr_qvs[qvs < 0, init := 'q']
ceu_fldr_qvs[qvs > 0, init := 'sm']
ceu_fldr_qvs[qvs == 0, init := 's']
ceu_fldr_qvs[, table(init), slope]

fldr_qvs_yr <- events_som[start_q != Inf & start_q != -Inf &
                            start_s != Inf & start_s != -Inf, 
                          .(qvs = start_q - start_s, slope, year, REG)]

to_plot <- fldr_qvs_yr[, .(mean_lag = mean(qvs)), .(slope, year, REG)] 
ggplot(to_plot, aes(year, as.numeric(mean_lag), col = REG)) +
  geom_smooth(se = F, span = 0.3) +
  theme_bw()


         
  
length(init[init>0]) / length(init)
length(init[init<0]) / length(init)
length(init[init==0]) / length(init)
events_som[ID == 213, start_p3 - start_pet ]
events_som[ID == 213]
