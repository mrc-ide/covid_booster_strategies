name <- "rq1_hic_drift_sensitivity"

df<- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  mutate(strategy_name = factor(
    strategy_name,
    levels = c(
      "Pre-vaccine introduction",
      "primary 10+, 3 doses only",
      "primary 10+, boost 75+ yearly",
      "primary 10+, boost 75+ 6 monthly",
      "primary 10+, boost 60+ yearly",
      "primary 10+, boost 60+ 6 monthly",
      "primary 10+, boost 10+ yearly"
    ),
    ordered = TRUE
  ))%>%
  filter(strategy_name %in% c("primary 10+, boost 60+ yearly"))

############################################################################
df_btotals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  mutate(strategy_name = factor(
    strategy_name,
    levels = c(
      "primary 10+, 3 doses only",
      "primary 10+, boost 75+ yearly",
      "primary 10+, boost 75+ 6 monthly",
      "primary 10+, boost 60+ yearly",
      "primary 10+, boost 60+ 6 monthly",
      "primary 10+, boost 10+ yearly"
    ),
    ordered = TRUE
  )) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_med/target_pop*1e6,
         hospitalisations = hosp_med/target_pop*1e6,
         deaths = deaths_med/target_pop*1e6) %>%
  select(target_pop, omicron_vaccine, strategy_name, mu_ab_infection, mu_ab_inf_scal_vfr, infections, deaths, hospitalisations) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths))

###############################
rq1_hosp <- ggplot(data = df, aes(x = as.Date(date), y = hosp_t/target_pop * 1e6, col = factor(rt_drift_factor))) +
  geom_ribbon(aes(ymin = hosp_tmin/target_pop * 1e6, ymax = hosp_tmax/target_pop * 1e6, fill = factor(rt_drift_factor)), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("#117733", "#88CCEE","#DDCC77", "#CC6677", "#882255"), labels = c("0%", "2.5%", "5%", "10%", "20%")) +
  scale_fill_manual(values = c("#117733", "#88CCEE","#DDCC77", "#CC6677", "#882255"), labels = c("0%", "2.5%", "5%", "10%", "20%")) +
  labs(x = "time", y = "daily hospitalisations\nper million", col = "drift level", fill =  "drift level")
rq1_hosp

rq1_inc <- ggplot(data = df, aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = factor(rt_drift_factor))) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = factor(rt_drift_factor)), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("#117733", "#88CCEE","#DDCC77", "#CC6677", "#882255"), labels = c("0%", "2.5%", "5%", "10%", "20%")) +
  scale_fill_manual(values = c("#117733", "#88CCEE","#DDCC77", "#CC6677", "#882255"), labels = c("0%", "2.5%", "5%", "10%", "20%")) +
  lims(x = c(as.Date("2021-01-01"), as.Date("2024-12-31"))) +
  labs(x = "time", y = "daily infections\nper million", col = "drift level", fill =  "drift level")

rq1_inc

plot_out <- rq1_hosp / rq1_inc +  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect")
plot_out
ggsave("plots/FigureS12sensitivity_drift.png", plot_out, height = 8, width = 10)
