library(ggplot2)

source("./4_analysis/rt_profiles.R")

df1$category <- "Category 1"
df2$category <- "Category 2"
df3$category <- "Category 3"

# Assuming your data frame is named 'df'
# 'date' represents the time points
# 'Transmission' represents the transmission values (Rt)

# Create a data frame for annotations
annotations <- read.csv("./data/dates_rt_plot.csv")
annotations$date <- as.Date(annotations$date)

# Create a data frame for immune escape annotations
immune_escape <- data.frame(
  label = c("VFR1 (Omicron)", "Drift begins", "New Variant"),  # Labels for immune escape dates
  date = c("2021-11-27", "2022-04-01", "2023-10-15"))  # Immune escape dates

immune_escape$date <- as.Date(immune_escape$date)


# Data frame with vaccine type dates 
vaccine_period <- read.csv("./data/vaccine_tye_rt_plot.csv")
vaccine_period$date_start <- as.Date(vaccine_period$date_start)
vaccine_period$date_end <- as.Date(vaccine_period$date_end)



plot_rt_annotated <- function(cat, df){
  
  
  vacc_period <- vaccine_period %>% filter(category == cat)
  annnot <- annotations %>% filter(category == cat)
  
  
  # Create the plot
  p <- ggplot() +
    annotate("rect", xmin = as.Date("2023-10-01"), xmax = as.Date("2023-11-01"), ymin = -0.2, ymax = 6, fill = "#1C7C54",alpha = 0.5) +
    geom_line(data= df, aes(x = date, y = Rt), size =1) +
    labs(x = "Time", y = "Transmission (Rt)") +
    # Add dose annotations
    geom_segment(
      data = annnot,
      aes(x = date, xend = date, y = 6.4, yend = 5.5),
      arrow = arrow(length = unit(0.25, "cm")),
      size=1,
      color = "#183A37"
    ) +
    geom_text(
      data = annnot,
      aes(x = date, y = 7.3, label = dose),
      vjust = 1.5,
      size = 3.5,
      position = position_nudge(x = c(-60,60, 0, -40, 30)),
      color="#183A37"
    ) + 
    # Add immune escape annotations
    geom_segment(
      data = immune_escape,
      aes(x = date, xend = date, y = 0.5, yend = -0.1),
      arrow = arrow(length = unit(0.25, "cm")),
      size=1,
      color = "#1C7C54"
    ) +
    geom_text(
      data = immune_escape,
      aes(x = date, y = 1.7, label = label),
      vjust = 1.5,
      color = "#1C7C54",
      position = position_nudge(x = c(-90,90, 120)),
      size = 3.5
    ) + 
    # Add vaccine period bars
    geom_rect(data = vacc_period,
              aes(xmin = as.Date(date_start), xmax = as.Date(date_end), ymin = 7.2, ymax = 7.7, fill = Vaccine),
              color = NA,
              alpha = 0.7) + scale_fill_manual(values = c("#A69F98", "#8C6057")) +
        # Customize the appearance of the plot
    theme_bw(base_size = 11) +  # Use a white background
    theme(panel.grid.major = element_blank(),
          strip.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.line = element_line(),
          plot.margin = margin(b = 1, t = 50, l = 10, r = 50),
          legend.position = "bottom") +  # Remove grid lines
    coord_cartesian(ylim = c(0, 6), xlim= c(as.Date("2020-01-01"), as.Date("2024-12-31")) ,clip="off") +
    annotate('text', 
             x = as.Date("2025-06-02"), y = 3, 
             label = cat,
             color='black',
            angle=270,
            size=5.5,
            fontface ="bold"
            )

    
  return(p) 
  
  
}



p1 <- plot_rt_annotated("Category 1", df1)
p2 <- plot_rt_annotated("Category 2", df2)
p3 <- plot_rt_annotated("Category 3", df3)


library(patchwork)
layout <- "
A
B
C
"
combined <- p1 + p2 + p3 +
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect", design = layout) &  theme(legend.position = 'bottom')

combined
ggsave("./plots/Rt_annotated.png", combined, height=10, width=8)

