cost1 <- 2
cost2 <- 20
cost3 <- 50

### main analysis for the LMIC setting ###

rq4_bv_drift <- readRDS("processed_outputs/df_summarise_totals_rq4_lmic_bv_drift.rds") %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

rq4_bv <- readRDS("processed_outputs/df_summarise_totals_rq4_lmic_bv.rds") %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

rq4_current <- readRDS("processed_outputs/df_summarise_totals_rq4_lmic_drift.rds") %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

### analysis for HIC new variant settings ###
rq4_newvar <- readRDS("processed_outputs/df_summarise_totals_rq4_lmic_newvariant.rds") %>%
  select(name, strategy_name, target_pop, variant_scenario, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)


# table of total events
totals_table_lmic_bv <- rq4_bv %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  mutate(strategy_name = factor(strategy_name, levels =c( "primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 40+ yearly", "primary 10+, boost 10+ yearly"), ordered = T)) %>%
  mutate(variant_scenario = NA) %>%
  arrange(name, strategy_name)

# table of total events
totals_table_lmic_current <- rq4_current %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  mutate(strategy_name = factor(strategy_name, levels =c( "primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 40+ yearly", "primary 10+, boost 10+ yearly"), ordered = T)) %>%
  mutate(variant_scenario = NA) %>%
  arrange(name, strategy_name)

# table of total events
totals_table_lmic_newvar <- rq4_newvar %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths) %>%
  filter((strategy_name == "primary 10+, boost 60+ yearly" & variant_scenario == "omicron-targeted vaccine") | (strategy_name =="primary 10+, boost 60+ yearly" & variant_scenario == "increased severity and immune escape" )| (strategy_name =="primary 10+, boost 10+ yearly" & variant_scenario == "increased severity and immune escape" )) %>%
  arrange(name, total_doses_med)

# table of total events
totals_table_lmic_drift_bv <- rq4_bv_drift %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
 # filter((strategy_name == "primary 10+, boost 60+ yearly" ) | (strategy_name == "primary 10+, boost 10+ yearly" ) ) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths) %>%
  arrange(name, total_doses_med) %>%
  mutate(variant_scenario = "none")

# table of total events
totals_table_lmic_drift_current <- rq4_drift %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  filter((strategy_name == "primary 10+, boost 60+ yearly" & omicron_vaccine == 0) | (strategy_name == "primary 10+, boost 10+ yearly" & omicron_vaccine == 0) ) %>%
  mutate(variant_scenario = paste0(strategy_name, "_", omicron_vaccine)) %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths) %>%
  arrange(name, total_doses_med)

# main totals - variant-adapted, with drift
totals_table_lmic_out <- rbind(totals_table_lmic_drift_bv, totals_table_lmic_newvar)
write_csv(totals_table_lmic_out, "tables/totals_table_lmic_rq4_TS8.csv")

# supplementary totals - variant-adapted, no drift
totals_table_lmic_out_supp <- rbind(totals_table_lmic_bv)
write_csv(totals_table_lmic_out_supp, "tables/totals_table_lmic_rq4_TS9.csv")

######################################################
### main analysis for the LMIC setting - WHO coverage ###

rq4_who <- readRDS("processed_outputs/df_summarise_totals_rq4_lmic_who.rds") %>%
  select(name, strategy_name, target_pop, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)

# table of total events
totals_table_lmic_who <- rq4_who %>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  select(name, strategy_name, total_doses_med, infections, hospitalisations, deaths)%>%
  mutate(strategy_name = factor(strategy_name, levels =c( "primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 40+ yearly", "primary 10+, boost 10+ yearly"), ordered = T)) %>%
  arrange(name, strategy_name)

write_csv(totals_table_lmic_who, "tables/totals_table_lmic_rq4_who_TS10.csv")

################################################################################

# table of events averted
rq4_bv_counter <- rq4_bv %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq4_bv_averted <- rq4_bv %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains(c("med", "upper", "lower")), names_to = "compartment") %>%
  left_join(rq4_bv_counter) %>%
  arrange(name, desc(strategy_name)) %>%
  mutate(variant_scenario = NA)


rq4_counter_newvar <- rq4_newvar %>%
  filter(strategy_name == "primary 10+, 3 doses only", variant_scenario != "no new variant") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq4_newvar_averted <- rq4_newvar %>%
  filter(variant_scenario == "increased severity and immune escape", strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment") %>%
  left_join(rq4_counter_newvar)

rq4_counter_bv_drift <- rq4_bv_drift %>%
  filter(strategy_name == "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment", values_to = "value_counter") %>%
  select(-strategy_name)

rq4_bv_drift_averted <- rq4_bv_drift %>%
  filter(strategy_name != "primary 10+, 3 doses only") %>%
  pivot_longer(cols = contains("med"), names_to = "compartment") %>%
  left_join(rq4_counter_bv_drift) %>%
  mutate(variant_scenario = paste0("omicron_vaccine_", "-")) 

table_out_lmic <- rbind(rq4_bv_averted, rq4_newvar_averted, rq4_bv_drift_averted) %>%
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

table_out_lmic_main <- table_out_lmic[c(4:8),]
table_out_lmic_supp <- table_out_lmic[c(1:3),]

write_csv(table_out_lmic_main, "tables/rq4_averted_T2.csv")
write_csv(table_out_lmic_supp, "tables/rq4_averted_TS12.csv")

### analysis for HIC new variant settings ###
rq4_newvar <- readRDS("processed_outputs/df_summarise_totals_rq4_lmic_newvariant.rds") %>%
  select(name, strategy_name, target_pop, variant_scenario, deaths_med, hosp_med, inc_med, total_doses_med) %>%
  pivot_longer(cols = contains("_med"), names_to = "compartment") %>%
  # scale to per million population
  mutate(value = round(value / target_pop * 1e6,0)) %>%
  pivot_wider(names_from = compartment, values_from = value) %>%
  select(-target_pop)%>%
  mutate(deaths = deaths_med,
         hospitalisations = hosp_med,
         infections = round(inc_med/1e3,0)) %>%
  filter(strategy_name == "primary 10+, boost 60+ yearly") %>%
  select(name, strategy_name, variant_scenario, total_doses_med, infections, hospitalisations, deaths) %>%
  arrange(name, total_doses_med)
write_csv(rq4_newvar, "tables/LMIC_newvar.csv")
