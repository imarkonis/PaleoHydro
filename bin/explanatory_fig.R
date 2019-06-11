source('./source/functions.R'); source('./source/graphics.R'); 

events_som <- readRDS('./data/events_som.Rds')
events <- readRDS('./data/events_met1.Rds')
dta <- readRDS('./data/timeseries_met1.Rds')
events_start <- events[, .SD[1], ID]
aa <- events_start[, .(ID, start_pet)]   #pet_start was not estimated in first data prep so we have to merge it here
events_som <- aa[events_som, on = 'ID']
events_som[, start_pet_month := month(start_pet)]
events_som[start_p3_month == 0 , start_p3_month := NA]

dr_event <- dta[PT_ID == 1223 & year(DTM) == 2003]
dr_event <- dr_event[, c(4, 12:15)]
colnames(dr_event) <- c('DTM', 'P', 'Q', 'S', 'PET')
to_plot <- melt(dr_event, id.vars = "DTM")
to_plot[, value_z := scale(value), variable]
ggplot(to_plot, aes(x = DTM, y = value, fill = variable)) +
  geom_col( position=position_dodge()) +
  scale_fill_manual(values = var_cols) +
  labs(title = "", x = "Month", y = "Deficit/excess volume (std.)") +
  guides(fill = guide_legend(title = "Variable")) +
  scale_x_date(breaks = 'months', date_labels = "%b") +
  theme_bw()

ggsave("./results/figs/dr_event.png", width = 6, height = 5)


