name <- "scenario1"
df <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))%>%
  mutate(scenario_name = factor(scenario_name, levels = c("no escape, no vaccine", "no escape, vaccine", "escape: no vaccine", "escape: unmatched vaccine", "escape: partially matched vaccine", "escape: matched vaccine")))
df_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

df_totals <- df_totals %>%
  mutate(scenario_name = factor(scenario_name, levels = c("no escape, no vaccine", "no escape, vaccine", "escape: no vaccine", "escape: unmatched vaccine", "escape: partially matched vaccine", "escape: matched vaccine")))

# smooth the incidence curves

p1 <- ggplot(data = df, aes(x = timestep-365*3, y = rollmean(inc_t, 7, na.pad = T), col = scenario_name)) +
  geom_line() +
  facet_wrap(~immune_escape, labeller = label_both) +
  th +
  scale_x_continuous(breaks = seq(0,365*5,365), limits = c(-183,365*2)) +
  scale_y_continuous(limits = c(0,1250)) +
  labs(x = "time (days)", y = "incidence", col = "scenario")
p1

ggsave("scenarios_output_inc.png", height = 5, width = 12)

p2 <- ggplot(data = df_totals, aes(x = scenario_name, y = inc_med, fill = scenario_name)) +
  geom_col()+
  labs(x = "scenario", y = "total incidence", fill = "scenario")+
  th +
  theme(axis.text.x = element_text(angle = 271, hjust = 1)) +
  coord_flip() +
  guides(fill = "none")

p2
ggsave("scenarios_output_totals.png", height = 5, width = 10)


