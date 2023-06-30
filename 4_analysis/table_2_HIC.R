cost1 <- 2
cost2 <- 20
cost3 <- 50

### main analysis for the two HIC settings (bivalent vaccine) ###
rq1_3_bv_drift <- readRDS("processed_outputs/df_summarise_totals_rq1_hic_bv_drift.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic_bv_drift.rds")) %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

rq1_3_bv <- readRDS("processed_outputs/df_summarise_totals_rq1_hic_bv.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic_bv.rds")) %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

### analysis for HIC new variant settings ###
rq1_3_newvar <- readRDS("processed_outputs/df_summarise_totals_rq1_hic_newvariant.rds") %>%
  mutate(variant_specific = 0,
         infection_decay_rate_scale = 1) %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic_newvariant.rds")) %>%
  select(name, strategy_name, target_pop, variant_scenario, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

# table of total events
totals_table_hic_bv_drift <- rq1_3_bv_drift %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  mutate(strategy_name = factor(strategy_name, levels =c( "primary 10+, 3 doses only", "primary 10+, boost 75+ yearly", "primary 10+, boost 75+ 6 monthly", "primary 10+, boost 60+ yearly", "primary 10+, boost 60+ 6 monthly", "primary 10+, boost 10+ yearly"), ordered = T)) %>%
  mutate(variant_scenario = NA) %>%
  arrange(name, strategy_name)

totals_table_hic_bv <- rq1_3_bv %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  mutate(strategy_name = factor(strategy_name, levels =c( "primary 10+, 3 doses only", "primary 10+, boost 75+ yearly", "primary 10+, boost 75+ 6 monthly", "primary 10+, boost 60+ yearly", "primary 10+, boost 60+ 6 monthly", "primary 10+, boost 10+ yearly"), ordered = T)) %>%
  mutate(variant_scenario = NA) %>%
  arrange(name, strategy_name)

# table of total events
totals_table_hic_newvar <- rq1_3_newvar %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths) %>%
  filter((strategy_name =="primary 10+, boost 60+ yearly" & variant_scenario == "increased severity and immune escape" )| (strategy_name =="primary 10+, boost 10+ yearly" & variant_scenario == "increased severity and immune escape" )) %>%
  arrange(name, total_doses_med)
  
totals_table_hic_main <- rbind(totals_table_hic_bv_drift, totals_table_hic_newvar)
totals_table_hic_main <- totals_table_hic_main[c(1:6, 13,14, 7:12,15,16),]
write_csv(totals_table_hic_main, "tables/totals_table_hic_TS6.csv")

totals_table_hic_supp <- totals_table_hic_bv
write_csv(totals_table_hic_supp, "tables/totals_table_hic_TS7.csv")

#######################################################################################################################

# tables of events averted
rq1_3_bv_counter <- rq1_3_bv %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq1_3_bv_averted <- rq1_3_bv %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment") %>%
  left_join(rq1_3_bv_counter) %>%
  arrange(name, desc(strategy_name)) %>%
  mutate(variant_scenario = NA)

rq1_3_bv_drift_counter <- rq1_3_bv_drift %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq1_3_bv_drift_averted <- rq1_3_bv_drift %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment") %>%
  left_join(rq1_3_bv_drift_counter) %>%
  arrange(name, desc(strategy_name)) %>%
  mutate(variant_scenario = NA)

rq1_3_counter_newvar <- rq1_3_newvar %>%
  filter(strategy_name == "primary 10+, 3 doses only", variant_scenario != "no new variant") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq1_3_newvar_averted <- rq1_3_newvar %>%
  filter(variant_scenario == "increased severity and immune escape", strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment") %>%
  left_join(rq1_3_counter_newvar)
  
table_out <- rbind(rq1_3_bv_averted, rq1_3_bv_drift_averted, rq1_3_newvar_averted) %>%
  mutate(events_averted = value_counter - value) %>%
  select(name, strategy_name, variant_scenario, compartment, events_averted) %>%
  pivot_wider(names_from = compartment, values_from = events_averted) %>%
  mutate(total_doses_med = -total_doses_med) %>%
  mutate(cost_per_hosp1 = round(cost1 * total_doses_med / hosp_med,0),
         cost_per_hosp2 = round(cost2 * total_doses_med / hosp_med,0),
         cost_per_hosp3 = round(cost3 * total_doses_med / hosp_med,0),
         cost_per_death1 = round(cost1 * total_doses_med / deaths_med,0),
         cost_per_death2 = round(cost2 * total_doses_med / deaths_med,0),
         cost_per_death3 = round(cost3 * total_doses_med / deaths_med,0)) %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0),
         doses_avert_hosp = round(total_doses_med/hosp_med,0),
         doses_avert_death = round(total_doses_med/deaths_med,0)
  ) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths, doses_avert_hosp, doses_avert_death, cost_per_hosp1, cost_per_hosp2, cost_per_hosp3, cost_per_death1, cost_per_death2, cost_per_death3)%>%
  arrange(name, total_doses_med)

# table_out_main <- table_out %>%
#   filter(!name %in% c("rq1_hic", "rq3_hic"),
#          variant_scenario %in% c("omicron_vaccine_1", NA, "increased severity and immune escape"))
#   
table_out_main <- table_out[c(6:12, 18:24),]
write_csv(table_out_main, "tables/rq1_3_averted_T1.csv")

table_out_supp <- table_out[c(1:5,13:17),]
write_csv(table_out_supp, "tables/rq1_3_averted_TS11.csv")

### analysis for HIC new variant settings ###
rq1_3_newvar <- readRDS("processed_outputs/df_summarise_totals_rq1_hic_newvariant.rds") %>%
  mutate(variant_specific = 0,
         infection_decay_rate_scale = 1) %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic_newvariant.rds")) %>%
  select(name, strategy_name, target_pop, variant_scenario, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop) %>%
  filter(strategy_name %in% "primary 10+, boost 60+ yearly")%>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths)
rq1_3_newvar
write_csv(rq1_3_newvar, "tables/HIC_newvar.csv")
