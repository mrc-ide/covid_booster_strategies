### additional analysis for the LMIC setting ###

rq2 <- readRDS("processed_outputs/df_summarise_totals_rq2_lmic_drift.rds") %>%
  select(name, strategy, strategy_name, target_pop, deaths_med,  hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = value / target_pop * 1e6) %>%
  pivot_wider(names_from = compartment, values_from = value)

# table of total events
totals_table_rq2 <- rq2 %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  arrange(name, total_doses_med)

write_csv(totals_table_rq2, "tables/totals_table_rq2_TS13.csv")
