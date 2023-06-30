name <- "rq3_hic_bv_drift"

df_bv <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_t,
         hospitalisations = hosp_t,
         deaths = deaths_t) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations, date, timestep) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(scenario = "variant-adapted vaccine")

df_totals_bv <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(scenario = "variant-adapted vaccine")

################################################

name <- "rq3_hic_drift"

df_baseline <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_t,
         hospitalisations = hosp_t,
         deaths = deaths_t) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations, date, timestep) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(scenario = "ancestral vaccine")

df_totals_baseline <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(scenario = "ancestral vaccine")

################################################

name <- "rq3_hic_continual_drift"

df_cont <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_t,
         hospitalisations = hosp_t,
         deaths = deaths_t) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations, date, timestep) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(scenario = "yearly updated vaccine")

df_totals_cont <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(target_pop, omicron_vaccine, strategy_name, infections, deaths, hospitalisations) %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
  mutate(scenario = "yearly updated vaccine")

############################################################

# combined totals plot
combined_df_totals <- rbind(df_totals_baseline,
                            df_totals_bv,
                            df_totals_cont) %>%
  mutate(scenario_name = paste0(strategy_name, ", ", scenario)) %>%
  filter(!(strategy_name == "primary 10+, 3 doses only" & scenario == "variant-adapted vaccine"),
         !(strategy_name == "primary 10+, 3 doses only" & scenario == "yearly updated vaccine")) %>%
  mutate(scenario_name = factor(scenario_name, levels = c("primary 10+, 3 doses only, ancestral vaccine",
                                                "primary 10+, boost 60+ yearly, ancestral vaccine",
                                                "primary 10+, boost 60+ yearly, variant-adapted vaccine",
                                                "primary 10+, boost 60+ yearly, yearly updated vaccine",
                                                "primary 10+, boost 10+ yearly, ancestral vaccine",
                                                "primary 10+, boost 10+ yearly, variant-adapted vaccine",
                                                "primary 10+, boost 10+ yearly, yearly updated vaccine")))

p1 <- ggplot(data = combined_df_totals, aes(x = scenario_name, y = value, fill = scenario_name)) +
  facet_wrap(~name, scales = "free") +
  geom_col(position = "dodge") +
  #geom_text(aes(label = round(value)), vjust = -0.5, position = position_dodge(0.9), size = 3) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank()) +
  labs(x = "vaccine strategy", y = "events per million", fill = "scenario") +
  scale_fill_manual(values = c("#332288", "#117733", "#44AA99", "#88CCEE", "#CC6677", "#AA4499", "#882255")) 
p1
table_out <- combined_df_totals %>%
  select(scenario, name, value, scenario_name) %>%
  pivot_wider(names_from = name, values_from = value)
############################################################

# combined trajectories plot

combined_df <- rbind(df_baseline,
                     df_bv,
                     df_cont) %>%
  mutate(scenario_name = paste0(strategy_name, ", ", scenario)) %>%
  filter(!(strategy_name == "primary 10+, 3 doses only" & scenario == "variant-adapted vaccine"),
         !(strategy_name == "primary 10+, 3 doses only" & scenario == "yearly updated vaccine"),
         name != "deaths") %>%
  mutate(scenario_name = factor(scenario_name, levels = c("primary 10+, 3 doses only, ancestral vaccine",
                                                          "primary 10+, boost 60+ yearly, ancestral vaccine",
                                                          "primary 10+, boost 60+ yearly, variant-adapted vaccine",
                                                          "primary 10+, boost 60+ yearly, yearly updated vaccine",
                                                          "primary 10+, boost 10+ yearly, ancestral vaccine",
                                                          "primary 10+, boost 10+ yearly, variant-adapted vaccine",
                                                          "primary 10+, boost 10+ yearly, yearly updated vaccine")))



combined_df <- rbind(df_baseline,
      df_bv,
      df_cont) %>%
  mutate(scenario_name = paste0(strategy_name, ", ", scenario)) %>%
  filter(!(strategy_name == "primary 10+, 3 doses only" & scenario == "variant-adapted vaccine"),
         !(strategy_name == "primary 10+, 3 doses only" & scenario == "yearly updated vaccine"),
         !name == "deaths") %>%
  mutate(scenario_name = factor(scenario_name, levels = c("primary 10+, 3 doses only, ancestral vaccine",
                                                          "primary 10+, boost 60+ yearly, ancestral vaccine",
                                                          "primary 10+, boost 60+ yearly, variant-adapted vaccine",
                                                          "primary 10+, boost 60+ yearly, yearly updated vaccine",
                                                          "primary 10+, boost 10+ yearly, ancestral vaccine",
                                                          "primary 10+, boost 10+ yearly, variant-adapted vaccine",
                                                          "primary 10+, boost 10+ yearly, yearly updated vaccine"))) #%>%
 
  

p2 <- ggplot(data = filter(combined_df, date >= "2022-01-01"), aes(x = date, y = value, col = scenario_name)) +
  facet_wrap(~name, scales = "free") +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(angle = 330, vjust = 0, hjust=0),
        legend.position = "none")+
  scale_color_manual(values = c("#332288", "#117733", "#44AA99", "#88CCEE", "#CC6677", "#AA4499", "#882255"), name = "scenario") +
  labs(x = "vaccine strategy", y = "events per million", color = "scenario") +
  guides(color=guide_legend()) +
 lims(x = c(as.Date("2022-01-01"), as.Date("2024-12-31")))

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
ggsave("plots/Figure5_rq3.png", combined, height = 8, width = 10)

write_csv(table_out, "tables/FigureS10benefit_variant_adapted_rq4.csv")

