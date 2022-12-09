cost1 <- 2
cost2 <- 20
cost3 <- 50

### main analysis for the two HIC settings (bivalent vaccine) ###
rq1_3_bv <- readRDS("processed_outputs/df_summarise_totals_rq1_hic_bv.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic_bv.rds")) %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

### main analysis for the two HIC settings (current vaccine) ###
rq1_3_current <- readRDS("processed_outputs/df_summarise_totals_rq1_hic.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic.rds")) %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

### analysis for HIC new variant settings ###
rq1_3_newvar <- readRDS("processed_outputs/df_summarise_totals_rq1_hic_newvariant.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic_newvariant.rds")) %>%
  select(name, strategy_name, target_pop, variant_scenario, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

### analysis for HIC variant drift settings ###
rq1_3_drift <- readRDS("processed_outputs/df_summarise_totals_rq1_hic_drift.rds") %>%
  rbind(readRDS("processed_outputs/df_summarise_totals_rq3_hic_drift.rds")) %>%
  filter(rt_drift_factor == 1.05) %>%
  select(name, strategy_name, target_pop, omicron_vaccine, rt_drift_factor, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

# table of total events
totals_table_hic_bv <- rq1_3_bv %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  mutate(strategy_name = factor(strategy_name, levels =c( "primary 10+, 3 doses only", "primary 10+, boost 75+ yearly", "primary 10+, boost 75+ 6 monthly", "primary 10+, boost 60+ yearly", "primary 10+, boost 60+ 6 monthly", "primary 10+, boost 10+ yearly"), ordered = T)) %>%
  mutate(variant_scenario = NA) %>%
  arrange(name, strategy_name)

totals_table_hic_current <- rq1_3_current %>%
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
  
  # table of total events
  totals_table_hic_drift <- rq1_3_drift %>%
    mutate(deaths = deaths_med,
           hospitalisations = hosp_med,
           infections = round(inc_med/1e3,0)) %>%
    filter((strategy_name == "primary 10+, boost 60+ yearly") | (strategy_name == "primary 10+, boost 10+ yearly") ) %>%
    mutate(variant_scenario = paste0(strategy_name, "_", omicron_vaccine)) %>%
    select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths) %>%
  arrange(name, total_doses_med)
  
totals_table_hic_main <- rbind(totals_table_hic_bv, totals_table_hic_newvar, totals_table_hic_drift)
totals_table_hic_main <- totals_table_hic_main[c(1:6, 13,17 , 14,20, 7:12, 15, 22, 16, 24),]
write_csv(totals_table_hic_main, "tables/totals_table_hic_TS6.csv")

totals_table_hic_supp <- rbind(totals_table_hic_current, totals_table_hic_drift)
totals_table_hic_supp <- totals_table_hic_supp[c(1:6, 14, 15, 7:12, 17, 19),]
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

rq1_3_current_counter <- rq1_3_current %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq1_3_current_averted <- rq1_3_current %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment") %>%
  left_join(rq1_3_current_counter) %>%
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

rq1_3_counter_drift <- rq1_3_drift %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name, -omicron_vaccine)

rq1_3_drift_averted <- rq1_3_drift %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment") %>%
  left_join(rq1_3_counter_drift) %>%
  mutate(variant_scenario = paste0("omicron_vaccine_", omicron_vaccine)) %>%
  select(-omicron_vaccine, -rt_drift_factor)
  
table_out <- rbind(rq1_3_bv_averted, rq1_3_current_averted, rq1_3_newvar_averted, rq1_3_drift_averted) %>%
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
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths, cost_per_hosp1, cost_per_hosp2, cost_per_hosp3, cost_per_death1, cost_per_death2, cost_per_death3)%>%
  arrange(name, total_doses_med)

table_out_main <- table_out %>%
  filter(!name %in% c("rq1_hic", "rq3_hic"),
         variant_scenario %in% c("omicron_vaccine_1", NA, "increased severity and immune escape"))
  
table_out_main <- table_out_main[c(1:5, 8, 6, 9, 7, 10:14, 17, 15, 18, 16),]
write_csv(table_out_main, "tables/rq1_3_averted_T1.csv")

table_out_supp <- table_out %>%
  filter(!name %in% c("rq1_hic_bv", "rq3_hic_bv"),
         variant_scenario %in% c("omicron_vaccine_0", NA))
write_csv(table_out_supp, "tables/rq1_3_averted_TS11.csv")

