#EDA for drought events as identified in init.R

source('./source/functions.R'); source('./source/graphics.R') 

events <- readRDS('./data/events_met1.Rds') #created in init.R

#consistency check of the duration distribution
round(events[REG == 'NEU', table(dur) / .N], 5) * 100
round(events[REG == 'CEU', table(dur) / .N], 5) * 100
round(events[REG == 'MED', table(dur) / .N], 5) * 100

round(events[REG == 'CEU' & year(start) <= 1900, table(dur) / .N], 5) * 100
round(events[REG == 'CEU' & year(start) > 1900, table(dur) / .N], 5) * 100

events[dur > 3 & REG == 'NEU' & year(start) > 2000, table(type) / .N]
events[dur > 3 & REG == 'NEU' & year(start) < 2000, table(type) / .N]
events[dur > 3 & REG == 'CEU' & year(start) > 2000, table(type) / .N]
events[dur > 3 & REG == 'CEU' & year(start) < 2000, table(type) / .N]
events[dur > 3 & REG == 'MED' & year(start) > 2000, table(type) / .N]
events[dur > 3 & REG == 'MED' & year(start) < 2000, table(type) / .N]

events[dur > 12 & REG == 'NEU', round(table(start_month) / .N, 2)]
events[dur > 12 & REG == 'CEU', round(table(start_month) / .N, 2)]
events[dur > 12 & REG == 'MED', round(table(start_month) / .N, 2)]

events[dur > 6 & REG == 'NEU' & year(start) > 2000, round(table(start_month) / .N, 2)]
events[dur > 6 & REG == 'NEU' & year(start) < 2000, round(table(start_month) / .N, 2)]
events[dur > 6 & REG == 'CEU' & year(start) > 2000, round(table(start_month) / .N, 2)]
events[dur > 6 & REG == 'CEU' & year(start) < 2000, round(table(start_month) / .N, 2)]
events[dur > 6 & REG == 'CEU' & year(start) > 2000, round(table(start_month) / .N, 2)]
events[dur > 6 & REG == 'CEU' & year(start) < 2000, round(table(start_month) / .N, 2)]

events[dur > 6 & REG == 'CEU' & year(start) > 2000, round(table(month(start_s)) / .N, 2)]
events[dur > 6 & REG == 'CEU' & year(start) > 2000, round(table(month(start_q)) / .N, 2)]
events[dur > 3 & dur < 12 & REG == 'CEU' & year(start) > 2000, round(table(month(start_diff)) / .N, 2)]
events[dur > 3 & dur < 12 & REG == 'CEU' & year(start) < 2000, round(table(month(start_diff)) / .N, 2)]
