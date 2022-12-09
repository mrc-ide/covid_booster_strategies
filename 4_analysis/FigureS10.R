name <- "rq1_hic_bv"

df_bv <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_t,
         hospitalisations = hosp_t,
         deaths = deaths_t) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations, date, timestep) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(vfr_drift_factor = "no drift")%>%
  mutate(scenario = "mRNA-1273.214")

df_totals_bv <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(vfr_drift_factor = "no drift")%>%
  mutate(scenario = "mRNA-1273.214")

################################################

name <- "rq1_hic"

df_baseline <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_t,
         hospitalisations = hosp_t,
         deaths = deaths_t) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations, date, timestep) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(vfr_drift_factor = "no drift")%>%
  mutate(scenario = "mRNA-1273")

df_totals_baseline <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(vfr_drift_factor = "no drift")%>%
  mutate(scenario = "mRNA-1273")

############################################################

# combined totals plot

combined_df_totals <- rbind(df_totals_baseline,
                     df_totals_bv) %>%
  mutate(omicron_vaccine = if_else(omicron_vaccine == 0, "mRNA-1273", "mRNA-1273.214")) %>%
  mutate(scenario = if_else(scenario == "drift scenario", paste0(scenario, ", ", omicron_vaccine), scenario)) %>%
  select(-vfr_drift_factor) %>%
  mutate(scenario = factor(scenario, levels = c("mRNA-1273", "mRNA-1273.214", "drift scenario, mRNA-1273", "drift scenario, mRNA-1273.214"))) %>%
  filter(!(strategy_name == "primary 10+, 3 doses only" & omicron_vaccine == "mRNA-1273.214")) %>%
  mutate(strategy_name = factor(strategy_name, levels = c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")))


combined_df_totals <- rbind(df_totals_baseline,
                            df_totals_bv) %>%
  mutate(omicron_vaccine = if_else(omicron_vaccine == 0, "mRNA-1273", "mRNA-1273.214")) %>%
  mutate(scenario = paste0(strategy_name, ", ", omicron_vaccine)) %>%
  select(-vfr_drift_factor) %>%
  filter(!(strategy_name == "primary 10+, 3 doses only" & omicron_vaccine == "mRNA-1273.214")) %>%
  mutate(scenario = factor(scenario, levels = c("primary 10+, 3 doses only, mRNA-1273",
                                                "primary 10+, boost 60+ yearly, mRNA-1273",
                                                "primary 10+, boost 60+ yearly, mRNA-1273.214", 
                                                "primary 10+, boost 10+ yearly, mRNA-1273",
                                                "primary 10+, boost 10+ yearly, mRNA-1273.214")))%>%
  filter(!(strategy_name == "primary 10+, 3 doses only" & omicron_vaccine == "mRNA-1273.214"))

p1 <- ggplot(data = combined_df_totals, aes(x = scenario, y = value, fill = scenario)) +
  facet_wrap(~name, scales = "free") +
  geom_col(position = "dodge") +
  #geom_text(aes(label = round(value)), vjust = -0.5, position = position_dodge(0.9), size = 3) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank()) +
  labs(x = "vaccine strategy", y = "events per million", fill = "scenario") +
  scale_fill_manual(values = c("black", "#2166AC", "#92C5DE", "#B2182B", "#F4A582")) 
p1

############################################################

# combined trajectories plot

combined_df <- rbind(df_baseline,
                     df_bv) %>%
  mutate(omicron_vaccine = if_else(omicron_vaccine == 0, "mRNA-1273", "mRNA-1273.214")) %>%
  mutate(scenario = paste0(strategy_name, ", ", omicron_vaccine)) %>%
  select(-vfr_drift_factor) %>%
  filter(!(strategy_name == "primary 10+, 3 doses only" & omicron_vaccine == "mRNA-1273.214"),
         name != "deaths") %>%
  mutate(scenario = factor(scenario, levels = c("primary 10+, 3 doses only, mRNA-1273",
                                                "primary 10+, boost 60+ yearly, mRNA-1273",
                                                "primary 10+, boost 60+ yearly, mRNA-1273.214", 
                                                "primary 10+, boost 10+ yearly, mRNA-1273",
                                                "primary 10+, boost 10+ yearly, mRNA-1273.214"))) %>%
  mutate(omicron_vaccine = factor(omicron_vaccine, levels = c("mRNA-1273", "mRNA-1273.214")))

p2 <- ggplot(data = filter(combined_df, date >= "2022-01-01"), aes(x = date, y = value, col = scenario)) +
  facet_wrap(~name, scales = "free") +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(angle = 330, vjust = 0, hjust=0),
        legend.position = "none")+
  scale_color_manual(values = c("black", "#2166AC", "#92C5DE", "#B2182B", "#F4A582"), name = "scenario") +
  labs(x = "vaccine strategy", y = "events per million", color = "scenario") +
  guides(color=guide_legend())
p2

library(patchwork)

layout <- "
A
B
C
"

combined <- p1 +
  p2 +
  guide_area() +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect", design = layout)
combined
ggsave("plots/FigureS10.png", combined, height = 8, width = 10)


