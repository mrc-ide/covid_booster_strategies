# set up transmission
R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")
end_date <- "12/01/2024"
tmax_date <- as.Date(x = end_date, format = "%m/%d/%Y")
time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))
vfr_time1 <- "11/27/2021"
vfr_time2 <- "12/31/2021"
vfr2_time1 <- "10/1/2022"
vfr2_time2 <- "10/31/2022"
max_Rt_var2_scal <- 1

df1 <- generate_Rt_hic(max_Rt_var2_scal = max_Rt_var2_scal, tmax_date = end_date,
                                                                       vfr2_time1 = vfr2_time1,
                                                                       vfr2_time2 = vfr2_time2,
                                                                       name = "rq1_hic")


df3 <- generate_Rt_hic_zerocovid(max_Rt_var2_scal = max_Rt_var2_scal,
                                                                                 tmax_date = end_date,
                                                                                 vfr2_time1 = vfr2_time1,
                                                                                 vfr2_time2 = vfr2_time2,
                                                                                 name = "rq3_hic")

df2 <- generate_Rt_lmic(max_Rt_var2_scal = 1,
                                                      tmax_date = end_date,
                                                      vfr2_time1 = vfr2_time1,
                                                      vfr2_time2 = vfr2_time2,
                                                      name = "rq4_lmic")




df1 <- df1 %>%
  mutate(date = Rt_tt + as.Date("2020-02-01"))
df2 <- df2 %>%
  mutate(date = Rt_tt + as.Date("2020-02-01"))
df3 <- df3 %>%
  mutate(date = Rt_tt + as.Date("2020-02-01"))

p_Rt1 <- ggplot(data = df1, aes(x = as.Date(date), y = Rt)) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none",
        plot.title = element_text(size=11)) +
  labs(x = "Time", y = "Rt", col = "", linetype = "")

p_Rt1


p_Rt2 <- ggplot(data = df2, aes(x = as.Date(date), y = Rt)) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none",
        plot.title = element_text(size=11)) +
  labs(x = "Time", y = "Rt", col = "", linetype = "")

p_Rt2

p_Rt3 <- ggplot(data = df3, aes(x = as.Date(date), y = Rt)) +
  geom_line(size = 0.8) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "right",
        plot.title = element_text(size=11)) +
  labs(x = "Time", y = "Rt", col = "", linetype = "")

p_Rt3

library(patchwork)
layout <- "
ABC
"
combined <- p_Rt1 + p_Rt2 + p_Rt3 +
  plot_annotation(tag_levels = "A") + 
  #plot_layout(guides = "collect") + 
  plot_layout(ncol = 3, design = layout)

combined
ggsave("plots/FigureS2.png", combined, height = 3, width = 9)
