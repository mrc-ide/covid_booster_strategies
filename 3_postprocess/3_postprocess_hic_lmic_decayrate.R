postprocess_hic_lmic_decayrate <- function(name){
  
  # get a few parameters we need - specify start of time period for when you want to calculate totals
  R0_t0 <- as.Date(x = "1/2/2020", format = "%d/%m/%Y")
  start_sum <- as.Date(x = "1/7/2022", format = "%d/%m/%Y")
  days_to_start_sum <- as.integer(difftime(start_sum, R0_t0))
  
  # read in scenarios
  scenarios <- read_csv(paste0("scenarios/scenarios_", name, ".csv"), show_col_types = FALSE)
  
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
      nest(cols = c(timestep, D_count, R_count, E_count, IMild_count, ICase_count, Rt, X1_count, X2_count, X3_count, X4_count, X5_count, X6_count, X7_count, X8_count, X9_count, incidence, vax_ab_mean, vax_ab_lower, vax_ab_upper, nat_ab_mean, nat_ab_lower, nat_ab_upper, nat_mean, nat_lower, nat_upper, sp_mean, sp_lower, sp_upper, hosp, hosp_all, ICU, ICU_all, doses)) 
    
    saf_reps_summarise$scenario <- scenario_num$scenario[i]
    df_all[[i]] <- saf_reps_summarise
  }
  
  # join the runs and link to parameters
  df_all <- do.call(rbind,df_all)
  
  df <- left_join(df_all, scenarios, by = "scenario") 
  
  # name the options
  df <- df %>%
    mutate(primary = "undefined",
           boost_age = "undefined",
           boost_frequency = "undefined",
           boost_age_scenario = "undefined") %>%
    mutate(
      primary = case_when(age_groups_covered == 15 ~ "primary 10+",
                          age_groups_covered == 16 ~ "primary 5+",
                          age_groups_covered == 17 ~ "primary 0+",
                          TRUE ~ primary),
      boost_age = case_when(age_groups_covered_d4 == 2 ~ "boost 75+",
                            age_groups_covered_d4 == 5 ~ "boost 60+",
                            age_groups_covered_d4 == 9 ~ "boost 40+",
                            age_groups_covered_d4 == 15 ~ "boost 10+",               
                            age_groups_covered_d4 == 16 ~ "boost 5+",
                            TRUE ~ boost_age),
      boost_frequency = case_when(vaccine_doses == 6 ~ "yearly",
                                  vaccine_doses == 8 ~ "6 monthly",
                                  vaccine_doses == 3 ~ "none",
                                  vaccine_doses == 5 ~ "yearly",
                                  TRUE ~ boost_frequency),
      boost_age_scenario = case_when(boost_age == "boost 75+" ~ "boost adults",
                                     boost_age == "boost 60+" ~ "boost adults",
                                     boost_age == "boost 10+" ~ "expand to children",
                                     boost_age == "boost 5+" ~ "expand to children",
                                     TRUE ~ boost_age_scenario)
    ) %>%
    mutate(strategy_name = paste0(primary, ", ", boost_age, " ", boost_frequency)) %>%
    mutate(strategy_name = if_else(strategy_name == "primary 10+, boost 75+ none", "primary 10+, 3 doses only", strategy_name),
           strategy_name = if_else(strategy_name == "primary 10+, boost 60+ none", "primary 10+, 3 doses only", strategy_name),
           strategy_name = if_else(strategy_name == "primary 5+, 3 doses only", "", strategy_name))
  
  # summarise totals over repetitions
  df <- df %>%
    group_by(income_group,
             target_pop,
             age_groups_covered,
             age_groups_covered_d3,
             age_groups_covered_d4,
             age_groups_covered_d5, 
             age_groups_covered_d6,
             age_groups_covered_d7,
             age_groups_covered_d8,
             age_groups_covered_d9,
             vaccine_doses,
             vacc_start,
             strategy_name,
             primary,
             boost_age,
             boost_frequency,
             boost_age_scenario,
             vacc_per_week, 
             t_d3, t_d4, t_d5, t_d6, t_d7, t_d8, t_d9,
             vfr,
             vfr2,
             vfr_time1,
             vfr_time2,
             vfr2_time1,
             vfr2_time2,
             mu_ab_infection,
             mu_ab_inf_scal_vfr,
             hosp_scal_vfr,
             ICU_scal_vfr,
             hosp_scal_vfr2,
             ICU_scal_vfr2,
             omicron_vaccine,
             vaccine_vfr,
             vfr_drift_factor,
             rt_drift_factor,
             infection_decay_rate_scale
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
    group_by(timestep,
             income_group,
             target_pop,
             age_groups_covered,
             age_groups_covered_d3,
             age_groups_covered_d4,
             age_groups_covered_d5, 
             age_groups_covered_d6,
             age_groups_covered_d7,
             age_groups_covered_d8,
             age_groups_covered_d9,
             vaccine_doses,
             vacc_start,
             strategy_name,
             primary,
             boost_age,
             boost_age_scenario,
             boost_frequency,
             vacc_per_week, 
             t_d3, t_d4, t_d5, t_d6, t_d7, t_d8, t_d9,
             vfr,
             vfr2,
             vfr_time1,
             vfr_time2,
             vfr2_time1,
             vfr2_time2,
             mu_ab_infection,
             mu_ab_inf_scal_vfr,
             hosp_scal_vfr,
             ICU_scal_vfr,
             hosp_scal_vfr2,
             ICU_scal_vfr2,
             omicron_vaccine,
             vaccine_vfr,
             vfr_drift_factor,
             rt_drift_factor,
             infection_decay_rate_scale
             ) %>%
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
              Rt = median(Rt),
              .groups = 'drop') %>%
    unique() %>%
    mutate(date = timestep + as.Date("2020-02-01"))
  
  # tidy up period before vacc introduction
  df0_pre_vacc <- df_summarise %>%
    filter(date < as.Date("2021-01-01"))# %>%
    # filter(strategy_name == "primary 10+, 3 doses only") %>%
    # mutate(strategy_name = "Pre-vaccine introduction")
  
  df0_post_vacc <- df_summarise %>%
    filter(date >= as.Date("2021-01-01"))
  
  df_summarise <- rbind(df0_pre_vacc, df0_post_vacc)
  
  if (name %in% c("rq1_hic_newvariant", "rq3_hic_newvariant", "rq4_lmic_newvariant", "rq1_hic_newvariant_old")) {
    df_summarise <- df_summarise %>%
      mutate(variant_scenario = "undefined") %>%
      mutate(
        variant_scenario = case_when(
            vfr2 == vfr & hosp_scal_vfr2 == hosp_scal_vfr ~ "no new variant",
            vfr2 == vfr & hosp_scal_vfr2 == 1 ~ "increased severity",
            vfr2 == 10 &
            hosp_scal_vfr2 == hosp_scal_vfr ~ "additional immune escape",
            vfr2 ==  10 &
            hosp_scal_vfr2 == 1 ~ "increased severity and immune escape",
          TRUE ~ variant_scenario
        )
      ) %>%
      mutate(variant_scenario = factor(
        variant_scenario,
        levels = c(
          "increased severity",
          "additional immune escape",
          "increased severity and immune escape",
          "no new variant"
        )
      ))
    
    df_summarise_totals <- df_summarise_totals %>%
      mutate(variant_scenario = "undefined") %>%
      mutate(
        variant_scenario = case_when(
            vfr2 == vfr & hosp_scal_vfr2 == hosp_scal_vfr ~ "no new variant",
            vfr2 == vfr & hosp_scal_vfr2 == 1 ~ "increased severity",
            vfr2 == 10 &
            hosp_scal_vfr2 == hosp_scal_vfr ~ "additional immune escape",
            vfr2 ==  10 &
            hosp_scal_vfr2 == 1 ~ "increased severity and immune escape",
          TRUE ~ variant_scenario
        )
      ) %>%
      mutate(variant_scenario = factor(
        variant_scenario,
        levels = c(
          "increased severity",
          "additional immune escape",
          "increased severity and immune escape",
          "no new variant"
        )
      ))
  }
  
  saveRDS(df_summarise, paste0("processed_outputs/df_summarise_", name, ".rds"))
  saveRDS(df_summarise_totals, paste0("processed_outputs/df_summarise_totals_", name, ".rds"))
}

