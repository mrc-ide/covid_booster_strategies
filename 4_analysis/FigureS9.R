name <- "rq4_lmic_who"

colset <- c(col_set_fig2[1], col_set_fig2[4], "tan4", col_set_fig2[6])

df_summarise <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_summarise_totals <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  mutate(strategy_name = factor(
    strategy_name,
    levels = c(
      "primary 10+, 3 doses only",
      "primary 10+, boost 60+ yearly",
      "primary 10+, boost 40+ yearly",
      "primary 10+, boost 10+ yearly"
    ),
    ordered = TRUE
  ))

# plot boosting vulnerable groups only: primary 10+
df1 <- df_summarise %>%
  mutate(strategy_name = factor(
    strategy_name,
    levels = c(
      "Pre-vaccine introduction",
      "primary 10+, 3 doses only",
      "primary 10+, boost 60+ yearly",
      "primary 10+, boost 40+ yearly",
      "primary 10+, boost 10+ yearly"
    ),
    ordered = TRUE
  ))
 
rq1_doses <- ggplot(data = filter(df1,strategy_name != "Pre-vaccine introduction"), aes(x = as.Date(date), y = vaccines_t/target_pop, col = strategy_name)) +
  geom_line(linewidth = 0.6) +
  lims(x = c(as.Date("2021-01-01"), as.Date("2024-12-31"))) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.position = "none") + 
  scale_color_manual(values = colset) +
  labs(x = "time", y = "cumulative doses\nper person", col = "dose strategy")
rq1_doses

rq1_hosp <- ggplot(data = df1, aes(x = as.Date(date), y = hosp_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = hosp_tmin/target_pop * 1e6, ymax = hosp_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", colset)) +
  scale_fill_manual(values = c("grey20", colset)) +
  labs(x = "time", y = "daily hospitalisations\nper million", col = "dose strategy", fill = "dose strategy")
rq1_hosp

rq1_inc <- ggplot(data = df1, aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = strategy_name)) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = strategy_name), alpha = 0.5, col = NA) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = c("grey20", colset)) +
  scale_fill_manual(values = c("grey20", colset)) +
 lims(x = c(as.Date("2020-01-01"), as.Date("2024-12-31"))) +
  labs(x = "time", y = "daily infections\nper million", col = "dose strategy", fill = "dose strategy")
  
rq1_inc

####################################################
# summary plots
df_summarydat <- df_summarise_totals %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(primary, boost_age, boost_frequency, strategy_name, deaths, infections, hospitalisations, target_pop)%>%
  pivot_longer(cols = c(infections, hospitalisations, deaths))

rq1_summary_plot <- ggplot(data = df_summarydat, aes(x = strategy_name, y = value / target_pop * 1e6, fill = strategy_name)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
  facet_wrap(~name, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank(),
        legend.text.align = 0) +
  scale_fill_manual(values = colset) +
  labs(x = "dose strategy", y = "total events per million", fill = "dose strategy")

rq1_summary_plot

######################################################
# plot additional events averted per additional vaccine dose
df_add1 <- df_summarise_totals %>%
  select(primary, boost_age, boost_frequency, strategy_name, deaths_med, inc_med, hosp_med, total_doses_med) %>%
  pivot_longer(cols = c(inc_med, deaths_med, hosp_med, total_doses_med))
df_add1_sub <- df_add1 %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  rename("value_counterfactual" = "value") %>%
  select(-c(primary, boost_age, boost_frequency, strategy_name))
df_add1 <- df_add1 %>%
  left_join(df_add1_sub) %>%
  mutate(difference = if_else(name == "total_doses_med", value - value_counterfactual, value_counterfactual - value)) %>%
  select(name, strategy_name, difference) %>%
  pivot_wider(names_from = name, values_from = difference) %>%
  mutate(infections = inc_med / total_doses_med * 100,
         hospitalisations = hosp_med / total_doses_med * 100,
         deaths = deaths_med / total_doses_med* 100) %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = c(infections, hospitalisations, deaths))

rq1_additional <- ggplot(data = df_add1, aes(x = strategy_name, y = value, fill = strategy_name)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name, scale = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank(),
        legend.text.align = 0,
        legend.position = "none") +
  labs(x = "dose strategy", y = "additional events averted\nper 100 additional doses", fill = "dose strategy") +
  scale_fill_manual(values = colset[2:4])

rq1_additional

######################################################
df1_nat <- df_summarise %>%
  mutate(titre = nat_med, nat_label = "all") %>%
  select(date, titre, strategy_name, nat_label)

df1_vax_ab <- df_summarise %>%
  mutate(titre = vax_ab_med, nat_label = "vaccination") %>%
  select(date, titre, strategy_name, nat_label)

df1_nat_ab <- df_summarise %>%
  mutate(titre = nat_ab_med, nat_label = "infection-induced") %>%
  select(date, titre, strategy_name, nat_label)

df1_nat <- rbind(df1_nat, df1_vax_ab, df1_nat_ab) %>%
  filter(strategy_name %in% c("primary 10+, boost 60+ yearly", "Pre-vaccine introduction")) %>%
  mutate(strategy_name = "primary 10+, boost 60+ yearly")

rq1_nat <- ggplot(data = df1_nat, aes(x = as.Date(date), y = titre, col = nat_label, linetype = nat_label)) +
  facet_wrap(~strategy_name) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_viridis_d(option = "A", begin = 0.2, end = 0.8) +
  scale_fill_viridis_d(option = "A", begin = 0.2, end = 0.8) +
  labs(x = "time", y = "immunity level", col = "IL measure", linetype = "IL measure")
rq1_nat

library(patchwork)

layout <- "
AB
CD
EE
FF
"

combined <- rq1_doses +
  rq1_nat +
  rq1_hosp +
  rq1_inc +
  rq1_summary_plot +
  rq1_additional +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect", design = layout)
combined
ggsave(paste0("plots/FigureS9_WHOcov", name, ".png"), combined, height = 10, width = 11)
