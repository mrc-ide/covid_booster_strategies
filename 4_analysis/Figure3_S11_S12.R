create_fig_s3_s11_s12 <- function(name = "rq1_hic_drift", filename = "Figure3"){
  col1a <- "#dadaeb"
  col1 <- "#6a51a3"
  col2a <- "#fcbba1"
  col2 <- "#cb181d"
  cols_pal_3 <- c(col1, col1a, col2, col2a)
  
  df_rq1 <- readRDS(paste0("processed_outputs/df_summarise_", name, ".rds"))
  df_totals_rq1 <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds"))
  
  # create figures rq1
  df_rq1 <- filter(df_rq1, strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly"),
                   (omicron_vaccine == 1 | (omicron_vaccine == 0 & strategy_name == "primary 10+, 3 doses only")),
                   date >= as.Date("2022/07/01", format = "%Y/%m/%d")) %>%
    mutate(scenario = paste0(strategy_name, vfr_drift_factor)) %>%
    mutate(vfr_drift_factor = factor(vfr_drift_factor, levels = c(1.05, 1.1), labels = c("5% drift", "10% drift")))
  
  df_totals_rq1 <- filter(df_totals_rq1, strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly"))
  
  rq1_hosp <- ggplot(data = df_rq1, aes(x = as.Date(date), y = hosp_t/target_pop * 1e6, col = factor(scenario))) +
    geom_ribbon(aes(ymin = hosp_tmin/target_pop * 1e6, ymax = hosp_tmax/target_pop * 1e6, fill = factor(scenario)), alpha = 0.7, col = NA) +
    facet_wrap(~vfr_drift_factor) +
    geom_line() +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.line = element_line(),
          legend.text.align = 0) +
    labs(x = "time", y = "daily hospitalisations\nper million", col = "variant scenario", fill = "variant scenario")+
    scale_color_manual(values = c("grey50", "grey20", col1, col2), labels = c("3 doses only, 5% drift", "3 doses only, 10% drift", "mRNA-1273.214, boost 60+ yearly, 5% drift", "mRNA-1273.214, boost 60+ yearly, 10% drift")) +
    scale_fill_manual(values = c("grey50", "grey20", col1, col2), labels = c("3 doses only, 5% drift", "3 doses only, 10% drift", "mRNA-1273.214, boost 60+ yearly, 5% drift", "mRNA-1273.214, boost 60+ yearly, 10% drift"))
  
  rq1_hosp
  
  rq1_inc <- ggplot(data = df_rq1, aes(x = as.Date(date), y = inc_t/target_pop * 1e6, col = factor(scenario))) +
    geom_ribbon(aes(ymin = inc_tmin/target_pop * 1e6, ymax = inc_tmax/target_pop * 1e6, fill = factor(scenario)), alpha = 0.7, col = NA) +
    facet_wrap(~vfr_drift_factor) +
    geom_line() +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.line = element_line(),
          legend.text.align = 0) +
    lims(x=c(as.Date("2022-07-01"), as.Date("2024-12-31"))) +
    labs(x = "Time", y = "daily infections per million", col = "variant scenario", fill = "variant scenario")+
    scale_color_manual(values = c("grey50", "grey20", col1, col2), labels = c("3 doses only, 5% drift", "3 doses only, 10% drift", "mRNA-1273.214, boost 60+ yearly, 5% drift", "mRNA-1273.214, boost 60+ yearly, 10% drift")) +
    scale_fill_manual(values = c("grey50", "grey20", col1, col2), labels = c("3 doses only, 5% drift", "3 doses only, 10% drift", "mRNA-1273.214, boost 60+ yearly, 5% drift", "mRNA-1273.214, boost 60+ yearly, 10% drift"))
  
  rq1_inc
  
  ####################################################
  # summary plots
  df_totals_rq1_pivot <- df_totals_rq1 %>%
    mutate(infections = inc_med,
           hospitalisations = hosp_med,
           deaths = deaths_med) %>%
    select(primary, boost_age, boost_frequency, strategy_name, deaths, infections, hospitalisations, target_pop, vfr_drift_factor, omicron_vaccine)%>%
    pivot_longer(cols = c(infections, hospitalisations, deaths)) %>%
    mutate(scenario = paste0(strategy_name, vfr_drift_factor, omicron_vaccine)) %>%
    mutate(vfr_drift_factor = factor(vfr_drift_factor, levels = c(1.05, 1.1), labels = c("5% drift", "10% drift")))
  
  
  col1a <- "#dadaeb"
  col1 <- "#6a51a3"
  col2a <- "#fcbba1"
  col2 <- "#cb181d"
  cols_pal_3 <- c(col1a, col1, col2a, col2)
  
  rq1_summary_plot <- ggplot(data = df_totals_rq1_pivot, aes(x = factor(scenario), y = value / target_pop * 1e6, fill = factor(scenario))) +
    geom_col(position = position_dodge2(width = 0.9, preserve = "single"))+
    facet_wrap(~name, scales = "free") +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.text.x = element_blank(),
          legend.text.align = 0) +
    labs(x = "scenario", y = "total events per million", fill = "scenario") +
    scale_fill_manual("scenario", values=c("grey50", "grey20", cols_pal_3), labels = c("3 doses only, 5% drift", "3 doses only, 10% drift", "mRNA-1273, boost 60+ yearly, 5% drift", "mRNA-1273.214, boost 60+ yearly, 5% drift", "mRNA-1273, boost 60+ yearly, 10% drift", "mRNA-1273.214, boost 60+ yearly, 10% drift"))
  
  rq1_summary_plot
  
  library(patchwork)
  combined_rq1 <- rq1_inc + rq1_summary_plot +
    plot_annotation(tag_levels = "A") + 
    plot_layout(guides = "collect") + 
    plot_layout(ncol = 1, nrow = 2)
  
  ggsave(paste0("plots/", filename, ".png"), combined_rq1, height = 6, width = 11)
}



