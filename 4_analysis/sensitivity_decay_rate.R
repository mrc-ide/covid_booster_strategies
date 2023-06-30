name <- "rq1_hic_decayrate"

df_bv_decay_low <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
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
  filter(strategy_name %in% c("primary 10+, boost 60+ yearly")) %>%
  filter(infection_decay_rate_scale == 0.5) %>%
  mutate(scenario = "50% decay rate post-infection") %>%
  select(-infection_decay_rate_scale)


df_bv_decay_mid <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
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
  filter(strategy_name %in% c("primary 10+, boost 60+ yearly")) %>%
  filter(infection_decay_rate_scale == 0.75) %>%
  mutate(scenario = "75% decay rate post-infection") %>%
  select(-infection_decay_rate_scale)

name <- "rq1_hic_bv_drift"

df_bv <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
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
  filter(strategy_name %in% c("primary 10+, boost 60+ yearly")) %>%
  mutate(scenario = "default")

df_combined <- rbind(df_bv, df_bv_decay_low, df_bv_decay_mid)

############################################################################
df_bv_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
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
  mutate(mu_ab_infection = factor(mu_ab_infection)) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths))

###############################
rq1_hosp <- ggplot(data = df_combined, aes(x = as.Date(date), y = hosp_t/target_pop * 1e6, col = factor(scenario))) +
  geom_ribbon(aes(ymin = hosp_tmin/target_pop * 1e6, ymax = hosp_tmax/target_pop * 1e6, fill = factor(scenario)), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_fig2[c(1,4)])) +
  scale_fill_manual(values = c("grey20", col_set_fig2[c(1,4)])) +
  labs(x = "time", y = "daily hospitalisations\nper million", col = "scenario", fill =  "scenario")
rq1_hosp

rq1_inc <- ggplot(data = df_combined, aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = factor(scenario))) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = factor(scenario)), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", col_set_fig2[c(1,4)])) +
  scale_fill_manual(values = c("grey20", col_set_fig2[c(1,4)])) +
  lims(x = c(as.Date("2020-01-01"), as.Date("2024-12-31"))) +
  labs(x = "time", y = "daily infections\nper million", col = "scenario", fill =  "scenario")

rq1_inc

plot_out <- rq1_hosp / rq1_inc +  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") 
  
plot_out
ggsave("plots/FigureS14sensitivitydecay.png", plot_out, height = 8, width = 10)
