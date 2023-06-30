name <- "rq1_hic_bv_drift"

df_totals_bv <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "variant-adapted vaccine")

################################################

name <- "rq1_hic_drift"

df_totals_baseline <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "ancestral vaccine")

################################################

name <- "rq1_hic_continual_drift"

df_totals_cont <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "yearly updated vaccine")

############################################################

# combined totals plot
combined_df_totals <- rbind(df_totals_baseline,
                            df_totals_bv,
                            df_totals_cont) %>%
  select(scenario, strategy_name, total_doses_med, infections, hospitalisations, deaths)
  

write_csv(combined_df_totals, "tables/benefit_variant_adapted_rq1.csv")


############################################################
############################################################

name <- "rq3_hic_bv_drift"

df_totals_bv <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "variant-adapted vaccine")

################################################

name <- "rq3_hic_drift"

df_totals_baseline <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "ancestral vaccine")

################################################

name <- "rq3_hic_continual_drift"

df_totals_cont <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "yearly updated vaccine")

############################################################

# combined totals plot
combined_df_totals <- rbind(df_totals_baseline,
                            df_totals_bv,
                            df_totals_cont) %>%
  select(scenario, strategy_name, total_doses_med, infections, hospitalisations, deaths)
write_csv(combined_df_totals, "tables/benefit_variant_adapted_rq3.csv")

############################################################
############################################################

name <- "rq4_lmic_bv_drift"

df_totals_bv <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "variant-adapted vaccine")

################################################

name <- "rq4_lmic_drift"

df_totals_baseline <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "ancestral vaccine")

################################################

name <- "rq4_lmic_continual_drift"

df_totals_cont <- readRDS(paste0("processed_outputs/df_summarise_totals_", name, ".rds")) %>%
  filter(strategy_name %in% c("primary 10+, 3 doses only", "primary 10+, boost 60+ yearly", "primary 10+, boost 10+ yearly")) %>%
  mutate(doses = round(total_doses_med,0),
         infections = round(inc_med/1000, 0),
         hospitalisations = round(hosp_med,0),
         deaths = round(deaths_med,0)) %>%
  select(target_pop, total_doses_med, strategy_name, infections, deaths, hospitalisations) %>%
  mutate(scenario = "yearly updated vaccine")

############################################################

# combined totals plot
combined_df_totals <- rbind(df_totals_baseline,
                            df_totals_bv,
                            df_totals_cont) %>%
  select(scenario, strategy_name, total_doses_med, infections, hospitalisations, deaths)
write_csv(combined_df_totals, "tables/benefit_variant_adapted_rq4.csv")
