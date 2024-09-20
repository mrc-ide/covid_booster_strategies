name <- "scenario1"
# read in scenarios

scenarios <- read_csv(paste0("scenarios/scenarios_", name, ".csv"), show_col_types = FALSE)

days_to_start_sum <- min(scenarios$vacc_start)

# read in raw outputs
df_all <- list.files(path = paste0("raw_outputs/output_", name, "/"), pattern = ".rds")
df_all <- map(paste0("raw_outputs/output_", name, "/", df_all), readRDS)
scenario_num <- data.frame(scenario_num = list.files(path = paste0("raw_outputs/output_", name, "/"), pattern = ".rds")) %>%
  separate(scenario_num, c("A", "scenario_num"), sep = "_") %>%
  separate(scenario_num, c("scenario", "B"), sep = ".rds") %>%
  select(scenario) %>%
  mutate(scenario = as.double(scenario))

# summarise each run
for (i in 1:length(df_all)){
  saf_reps_summarise <- df_all[[i]] %>%
    mutate(IMild_count = IMild_count + IAsymp_count) %>%
    dplyr::select(-IAsymp_count) %>%
    pivot_longer(cols = contains(c("count", "Rt","incidence","nat","sp", "vax","hosp","ICU")), names_to = "compartment") %>%
    filter(compartment %in% c("D_count", "X1_count", "X2_count", "X3_count", "X4_count", "X5_count", "X6_count", "X7_count", "X8_count", "X9_count","R_count","E_count", "IMild_count", "ICase_count", "Rt", "incidence","vax_ab_mean", "vax_ab_lower", "vax_ab_upper", "nat_ab_mean", "nat_ab_lower", "nat_ab_upper", "nat_mean", "nat_lower","nat_upper","sp_mean","sp_lower", "sp_upper", "hosp", "hosp_all", "ICU", "ICU_all" )) %>%
    group_by(compartment) %>%
    mutate(value = if_else(compartment == "D_count", value - lag(value), value),
           value = if_else(is.na(value), 0, value)) %>%
    ungroup() %>%
    pivot_wider(id_cols = timestep, names_from = "compartment", values_from = "value")  %>%
    mutate(deaths = sum(D_count[timestep >= days_to_start_sum]),
           cum_hosp = sum(hosp[timestep >= days_to_start_sum]),
           cum_hosp_all = sum(hosp_all[timestep >= days_to_start_sum]),
           cum_ICU = sum(ICU[timestep >= days_to_start_sum]),
           cum_ICU_all = sum(ICU_all[timestep >= days_to_start_sum]),
           inc = sum(incidence[timestep >= days_to_start_sum])) %>%
    ungroup()
  
  saf_reps_summarise <- add_cols("X1_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X2_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X3_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X4_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X5_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X6_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X7_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X8_count", saf_reps_summarise)
  saf_reps_summarise <- add_cols("X9_count", saf_reps_summarise)
  
  saf_reps_summarise <- saf_reps_summarise %>%
    mutate(doses = X1_count + X2_count * 2 + X3_count * 3 + X4_count * 4 + X5_count * 5 + X6_count * 6 + X7_count * 7 + X8_count * 8 + X9_count * 9,
           total_doses = max(doses)) %>%
    nest(cols = c(timestep, D_count, R_count, E_count, IMild_count, ICase_count, X1_count, X2_count, X3_count, X4_count, X5_count, X6_count, X7_count, X8_count, X9_count, incidence, vax_ab_mean, vax_ab_lower, vax_ab_upper, nat_ab_mean, nat_ab_lower, nat_ab_upper, nat_mean, nat_lower, nat_upper, sp_mean, sp_lower, sp_upper, hosp, hosp_all, ICU, ICU_all, doses)) 
  
  saf_reps_summarise$scenario <- scenario_num$scenario[i]
  df_all[[i]] <- saf_reps_summarise
}

# join the runs and link to parameters
df_all <- do.call(rbind,df_all)

df <- left_join(df_all, scenarios, by = "scenario") 


# summarise totals over repetitions
df <- df %>%
  group_by(scenario, scenario_name
  ) %>%
  mutate(deaths_med = median(deaths),
         deaths_lower = quantile(deaths, 0.025),
         deaths_upper = quantile(deaths, 0.975),
         hosp_med = median(cum_hosp),
         hosp_lower = quantile(cum_hosp, 0.025),
         hosp_upper = quantile(cum_hosp, 0.975),
         icu_med = median(cum_ICU),
         icu_lower = quantile(cum_ICU, 0.025),
         icu_upper = quantile(cum_ICU, 0.975),
         inc_med = median(inc),
         inc_lower = quantile(inc, 0.025),
         inc_upper = quantile(inc, 0.975),
         total_doses_med = median(total_doses)) %>%
  ungroup() 

df_summarise_totals <- df %>%
  select(-c(deaths, cum_hosp, cum_hosp_all, cum_ICU, cum_ICU_all, inc, total_doses, cols, repetition, scenario)) %>%
  unique()

# summarise temporal dynamics over repetitions
df_summarise <- df %>%
  unnest(cols) %>%
  select(-c(deaths, cum_hosp, cum_hosp_all, cum_ICU, cum_ICU_all, total_doses, inc)) %>%
  group_by(scenario_name, scenario, timestep) %>%
  summarise(deaths_t = median(D_count),
            deaths_tmin = quantile(D_count, 0.025),
            deaths_tmax = quantile(D_count, 0.975),
            hosp_t = median(hosp),
            hosp_tmin = quantile(hosp, 0.025),
            hosp_tmax = quantile(hosp, 0.975),
            ICU_t = median(ICU),
            ICU_tmin = quantile(ICU, 0.025),
            ICU_tmax = quantile(ICU, 0.975),
            E_t = median(E_count),
            IMild_t = median(IMild_count),
            ICase_t = median(ICase_count),
            inc_t = median(incidence),
            inc_tmin = quantile(incidence, 0.025),
            inc_tmax = quantile(incidence, 0.975),
            vax_ab_med = median(vax_ab_mean),
            vax_ab_lower = median(vax_ab_lower),
            vax_ab_upper = median(vax_ab_upper),
            nat_ab_med = median(nat_ab_mean),
            nat_ab_lower = median(nat_ab_lower),
            nat_ab_upper = median(nat_ab_upper),
            nat_med = median(nat_mean),
            nat_lower = median(nat_lower),
            nat_upper = median(nat_upper),
            sp_med = median(sp_mean),
            sp_lower = median(sp_lower),
            sp_upper = median(sp_upper),
            vaccines_t = median(X1_count + X2_count * 2 + X3_count * 3 + X4_count * 4 
                                + X5_count * 5 + X6_count * 6 + X7_count * 7 +
                                  X8_count * 8 + X9_count * 9),
            dose1_t = median(X1_count),
            dose2_t = median(X2_count),
            dose3_t = median(X3_count),
            dose4_t = median(X4_count),
            dose5_t = median(X5_count),
            dose6_t = median(X6_count),
            dose7_t = median(X7_count),
            dose8_t = median(X8_count),
            dose9_t = median(X9_count),
            .groups = 'drop') %>%
  unique() %>%
  left_join(scenarios)



saveRDS(df_summarise, paste0("processed_outputs/df_summarise_", name, ".rds"))
saveRDS(df_summarise_totals, paste0("processed_outputs/df_summarise_totals_", name, ".rds"))
