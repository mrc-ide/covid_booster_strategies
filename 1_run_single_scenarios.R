library(safirimmunity)

#### Set up simulation parameters ##############################################
name <- "scenario2"
rt <- 10
max_t <- 365*5
income_group <- "HIC"
target_pop <- 1e5
hs_constraints <- "Absent"
dt <- 0.5
seeding_cases <- 10
age_groups_covered <- 5
max_coverage <- 0.8
vacc_period <- 60
vacc_start <- 1095+60
vaccine_doses <- 1
vfr <- 5.2
vfr2 <- 5.2
vfr_time1 <- 1
vfr_time2 <- 30
vfr2_time1 <- 1095 # wont have any effect if vfr2 <- vfr, hosp_scale_vfr <- hosp_scale_vfr2 and ICU_scal_vfr <- ICU_scal_vfr2
vfr2_time2 <- 1095+90

vaccine <- "vaccine"
vacc_on <- c(1,0)

mu_ab_d1 = 1.13#log10(1.13)
mu_ab_d2 = 1.13#log10(1.13)
k = 3.1
hl_s = 35
hl_l = 1000
period_s <- 75
period_l <- 565 # not used
ab_50 = 0.04#log10(0.04)
ab_50_severe = 0.005#log10(0.005)
std10 = 0.44
immune_escape <- c(0,1)
matched_vacc <- c(0,1)
matched_vacc_level <- c(0.5,1)
max_ab <- 5

mu_ab_infection <- 1.13
std10_infection <- 0.44
mu_ab_inf_scal_vfr <- 1

age_groups_covered_d3 <- 14
age_groups_covered_d4 <- 14
age_groups_covered_d5 <- 14
repetition <- 1

#### Create scenarios ##########################################################
scenarios <- expand_grid(
  vacc_on,
  immune_escape,
  matched_vacc,
  matched_vacc_level,
  vfr2,
  name,
  rt,
  max_t,
  income_group,
  target_pop,
  hs_constraints,
  dt,
  seeding_cases,
  age_groups_covered,
  age_groups_covered_d3,
  age_groups_covered_d4,
  age_groups_covered_d5,
  max_coverage,
  vacc_period,
  vacc_start,
  vaccine_doses,
  vfr,
  vfr_time1,
  vfr_time2,
  vfr2_time1,
  vfr2_time2,
  vaccine,
  mu_ab_d1,
  mu_ab_d2,
  k,
  hl_l,
  hl_s,
  period_s,
  period_l,
  ab_50,
  ab_50_severe,
  std10,
  max_ab,
  mu_ab_infection,
  std10_infection,
  mu_ab_inf_scal_vfr,
  repetition) %>%
  filter((matched_vacc == 0 & matched_vacc_level == 1) | matched_vacc == 1) %>%
  filter((vacc_on == 0 & matched_vacc == 0) | vacc_on == 1) %>%
  filter((immune_escape == 0 & vacc_on == 0) | (immune_escape == 1 ) | (immune_escape == 0 & vacc_on == 1 & matched_vacc == 0)) %>%
  mutate(vacc_start = if_else(vacc_on == 0, max_t, vacc_start)) %>%
  mutate(vfr2 = if_else(immune_escape == 1, 10, vfr2))

nrow(scenarios)

# name the scenarios
scenarios <- scenarios %>%
  mutate(scenario_name = "undefined") %>%
  mutate(
    scenario_name = case_when(
      immune_escape == 0 & vacc_on == 0 ~ "no escape, no vaccine",
      immune_escape == 0 & vacc_on == 1 ~ "no escape, vaccine",
      immune_escape == 1 & vacc_on == 0 ~ "escape: no vaccine",
      immune_escape == 1 & vacc_on == 1 & matched_vacc == 0 ~ "escape: unmatched vaccine",
      immune_escape == 1 & vacc_on == 1 & matched_vacc == 1  & matched_vacc_level == 0.5 ~ "escape: partially matched vaccine",
      immune_escape == 1 & vacc_on == 1 & matched_vacc == 1 & matched_vacc_level == 1 ~ "escape: matched vaccine",
      TRUE ~ scenario_name))

scenarios <- scenarios %>%
  mutate(scenario = 1:nrow(scenarios))

write_csv(scenarios, paste0("scenarios/scenarios_", name, ".csv"))

#### Test on PC ###############################################################

source("R/run_function_immunity_new.R")
source("R/utils.R")
source("R/vaccine_strategy.R")

plan(multicore, workers = 4)
system.time({out <- future_pmap(scenarios, run_scenario_new, .progress = TRUE)})
