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

#CEU

ceu_fldr_init <- events_som[hclust == 'CEU_1', 
                            .(start_pet_month, start_p3_month, start_p3, start_pet, slope,
                              year, s_dur_rel = s_dur_tot/dur, q_dur_rel = q_dur_tot/dur)]
ceu_fldr_init[!is.na(start_pet_month) & !is.na(start_p3_month), pvt := start_p3 - start_pet]
ceu_fldr_init[, .(mean_lag = mean(pvt)), slope] 
ceu_fldr_init[pvt < 0 | is.na(start_pet_month), init := 'P']
ceu_fldr_init[pvt > 0 | is.na(start_p3_month), init := 'T']
ceu_fldr_init[pvt == 0, init := 'P & T']
ceu_fldr_init[, table(init), slope]
ceu_fldr_init[, decade := cut(year, breaks = seq(1900, 2030, 10), 
                              labels = as.character(seq(1900, 2020, 10)))]
ceu_fldr_init[, n_events := .N, decade]
to_plot <- unique(ceu_fldr_init[, .(pvt_f = .N/n_events), .(init, decade)])
to_plot <- to_plot[complete.cases(to_plot)]

g1 <- ggplot(to_plot, aes(decade, pvt_f, fill = init)) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values = period_cols) +
  labs(title = "CEU", x = "", y = "P vs T fraction") +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), alpha = 0.2) + 
  guides(fill = guide_legend(title = "Initiated by:")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.45, 12.55), ylim = c(0.04, 1))

to_plot <- unique(ceu_fldr_init[, .(s_dur_rel = mean(s_dur_rel), q_dur_rel = mean(q_dur_rel)), .(init, decade)])
to_plot[, decade := as.numeric(as.character(decade))]
to_plot <- to_plot[complete.cases(to_plot)]

g1b <- ggplot(to_plot, aes(decade, s_dur_rel, col = init, fill = init)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.3) +
  geom_smooth(alpha = 0.2) +
  scale_color_manual(values = period_cols) +
  scale_fill_manual(values = period_cols) +
  labs(x = "Time (decade)", y = "SM drought fraction") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

#NEU

neu_fldr_init <- events_som[hclust == 'NEU_3', 
                            .(start_pet_month, start_p3_month, start_p3, start_pet, slope, 
                              year, s_dur_rel = s_dur_tot/dur, q_dur_rel = q_dur_tot/dur)]
neu_fldr_init[!is.na(start_pet_month) & !is.na(start_p3_month), pvt := start_p3 - start_pet]
neu_fldr_init[, .(mean_lag = mean(pvt)), slope] 
neu_fldr_init[pvt < 0 | is.na(start_pet_month), init := 'P']
neu_fldr_init[pvt > 0 | is.na(start_p3_month), init := 'T']
neu_fldr_init[pvt == 0, init := 'P & T']
neu_fldr_init[, table(init), slope]
neu_fldr_init[, decade := cut(year, breaks = seq(1900, 2030, 10), 
                              labels = as.character(seq(1900, 2020, 10)))]
neu_fldr_init[, n_events := .N, decade]
to_plot <- unique(neu_fldr_init[, .(pvt_f = .N/n_events), .(init, decade)])
to_plot <- to_plot[complete.cases(to_plot)]

g2 <- ggplot(to_plot, aes(decade, pvt_f, fill = init)) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values = period_cols) +
  labs(title = "NEU", x = "", y = "") +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), alpha = 0.2) + 
  guides(fill = guide_legend(title = "Initiated by:")) +
  theme_minimal() +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.8, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.45, 12.55), ylim = c(0.04, 1))

to_plot <- unique(neu_fldr_init[, .(s_dur_rel = mean(s_dur_rel), q_dur_rel = mean(q_dur_rel)), .(init, decade)])
to_plot[, decade := as.numeric(as.character(decade))]
to_plot <- to_plot[complete.cases(to_plot)]

g2b <- ggplot(to_plot, aes(decade, s_dur_rel, col = init, fill = init)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.3) +
  geom_smooth(alpha = 0.2) +
  scale_color_manual(values = period_cols) +
  scale_fill_manual(values = period_cols) +
  labs(x = "Time (decade)", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, 0.8, "cm"))


#MED

med_fldr_init <- events_som[hclust == 'MED_7', 
                            .(start_pet_month, start_p3_month, start_p3, start_pet, slope,
                              year, s_dur_rel = s_dur_tot/dur, q_dur_rel = q_dur_tot/dur)]
med_fldr_init[!is.na(start_pet_month) & !is.na(start_p3_month), pvt := start_p3 - start_pet]
med_fldr_init[, .(mean_lag = mean(pvt)), slope] 
med_fldr_init[pvt < 0 | is.na(start_pet_month), init := 'P']
med_fldr_init[pvt > 0 | is.na(start_p3_month), init := 'T']
med_fldr_init[pvt == 0, init := 'P & T']
med_fldr_init[, table(init), slope]
med_fldr_init[, decade := cut(year, breaks = seq(1900, 2030, 10), 
                              labels = as.character(seq(1900, 2020, 10)))]
med_fldr_init[, n_events := .N, decade]
to_plot <- unique(med_fldr_init[, .(pvt_f = .N/n_events), .(init, decade)])
to_plot <- to_plot[complete.cases(to_plot)]

g3 <- ggplot(to_plot, aes(decade, pvt_f, fill = init)) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values = period_cols) +
  labs(title = "MED", x = "", y = "") +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), alpha = 0.2) + 
  guides(fill = guide_legend(title = "Initiated by:")) +
  theme_minimal() +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.8, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.45, 12.55), ylim = c(0.04, 1))

to_plot <- unique(med_fldr_init[, .(s_dur_rel = mean(s_dur_rel), q_dur_rel = mean(q_dur_rel)), .(init, decade)])
to_plot[, decade := as.numeric(as.character(decade))]
to_plot <- to_plot[complete.cases(to_plot)]

g3b <- ggplot(to_plot, aes(decade, s_dur_rel, col = init, fill = init)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.3) +
  geom_smooth(alpha = 0.2) +
  scale_color_manual(values = period_cols) +
  scale_fill_manual(values = period_cols) +
  labs(x = "Time (decade)", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, 0.8, "cm"))

gg_all <- ggarrange(g1, g3, g2,
                    g1b, g3b, g2b,
                    labels = c("a", "", '', 'b', '', ''),
                    common.legend = T, legend = 'right', 
                    nrow = 2, ncol = 3)

ggsave("./results/figs/summer_dr_pvt.png", width = 9, height = 5)



#Plot per year

fldr_init_yr <- events_som[!is.na(start_pet_month) & !is.na(start_p3_month), 
                               .(pvt = start_p3 - start_pet, slope, year, REG)]

to_plot <- fldr_init_yr[, .(mean_lag = mean(pvt)), .(slope, year, REG)] 
ggplot(to_plot, aes(year, as.numeric(mean_lag), col = REG)) +
  geom_line() +
  geom_smooth(se = F, span = 0.5) +
  theme_bw()

## Hydrological vs Agricultural Drought

ceu_fldr_qvs <- events_som[start_q != Inf & start_q != -Inf &
                           start_s != Inf & start_s != -Inf &
                           hclust == 'CEU_1', .(qvs = start_q - start_s, slope)]

ceu_fldr_qvs[, .(mean_lag = mean(qvs, na.rm = T)), slope] 
ceu_fldr_qvs[qvs < 0, init := 'q']
ceu_fldr_qvs[qvs > 0, init := 'sm']
ceu_fldr_qvs[qvs == 0, init := 's']
ceu_fldr_qvs[, table(init), slope]

fldr_qvs_yr <- events_som[start_q != Inf & start_q != -Inf &
                            start_s != Inf & start_s != -Inf &
                            (hclust == 'CEU_1' | hclust == 'MED_7' | hclust == 'NEU_4'), 
                          .(qvs = start_q - start_s, slope, year, hclust = factor(hclust))]

fldr_qvs_yr[, decade := cut(year, breaks = seq(1900, 2030, 10), 
                            labels = as.character(seq(1900, 2020, 10)))]
fldr_qvs_yr[, n_events := .N, decade]

to_plot <- fldr_qvs_yr[qvs < 0, .(mean_lag = mean(qvs)), .(hclust, decade)] 
to_plot[, mean_lag := as.numeric(to_plot$mean_lag)]
to_plot[, decade := as.numeric(as.character(decade))]
to_plot <- to_plot[complete.cases(to_plot)]

ggplot(to_plot, aes(decade, mean_lag, col = hclust)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = period_cols) +
  labs(x = "Time (decade)", y = "") +
  guides(col = guide_legend(title = "Region")) +
  theme_bw() 
