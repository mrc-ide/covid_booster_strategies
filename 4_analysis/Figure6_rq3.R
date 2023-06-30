c1 <- "#a67243"
c2 <- "#ffc8b3"
c3 <- "#1b463c"
c4 <- "#75c2b0"
figcols <- wes_palette("BottleRocket2", n = 5)
cols <- scales::viridis_pal(option = "E", begin = 0.2, end = 0.9)(5)

name <- "rq3_hic_newvariant"
df_rq3 <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
df_totals_rq3 <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))

# create figures rq3
df_rq3 <- filter(df_rq3, strategy_name == "primary 10+, boost 60+ yearly",
                 date >= as.Date("2022/07/01", format = "%Y/%m/%d"))
df_totals_rq3 <- filter(df_totals_rq3, strategy_name == "primary 10+, boost 60+ yearly")

rq3_hosp <- ggplot(data = df_rq3, aes(x = as.Date(date), y = hosp_t/target_pop * 1e6, col = variant_scenario)) +
  geom_ribbon(aes(ymin = hosp_tmin/target_pop * 1e6, ymax = hosp_tmax/target_pop * 1e6, fill = variant_scenario), alpha = 0.7, col = NA) +
  facet_wrap(~strategy_name) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_fill_manual(values = figcols) +
  scale_color_manual(values = figcols) +
  labs(x = "time", y = "daily hospitalisations\nper million", col = "", fill = "")
rq3_hosp

rq3_inc <- ggplot(data = df_rq3, aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = variant_scenario)) +
  geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = variant_scenario), alpha = 0.7, col = NA) +
  facet_wrap(~strategy_name) +
  geom_line() +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_fill_manual(values = figcols) +
  scale_color_manual(values = figcols) +
    lims(x = c(as.Date("2022-07-01"), as.Date("2024-12-31"))) +
  labs(x = "time", y = "daily infections per million", col = "", fill = "")

rq3_inc

####################################################
# summary plots
df_totals_rq3_pivot <- df_totals_rq3 %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(primary, boost_age, boost_frequency, strategy_name, deaths, infections, hospitalisations, target_pop, variant_scenario)%>%
  pivot_longer(cols = c(infections, hospitalisations, deaths))

rq3_summary_plot <- ggplot(data = df_totals_rq3_pivot, aes(x = variant_scenario, y = value / target_pop * 1e6, fill = variant_scenario)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"), alpha = 0.7)+
  facet_wrap(~name, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_blank(),
        legend.text.align = 0,
        legend.position = "none") +
  scale_fill_manual(values = figcols) +
  labs(x = "variant scenario", y = "total events per million", fill = "")

rq3_summary_plot

#########################
# combine plots
library(patchwork)
layout <- "
AB
CC
"

combined <- rq3_hosp + rq3_inc + rq3_summary_plot  + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect") + 
  plot_layout(ncol = 2, nrow = 2, design = layout) &
  theme(legend.position = 'bottom',
        legend.box = 'vertical')

combined
ggsave(paste0("plots/FigureS11.png"),combined, height = 7, width = 10)

tab_rq3 <- df_totals_rq3 %>%
  mutate(infections = inc_med,
         hospitalisations = hosp_med,
         deaths = deaths_med) %>%
  select(strategy_name, infections, hospitalisations, deaths, variant_scenario) %>%
  mutate(category = 2)

write_csv(tab_rq3, "tables/totals_figure6_rq3.csv")

