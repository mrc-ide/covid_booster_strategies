name <- "rq3_hic_drift"

#### Get vaccine parameters  ##############################################
vaccine <- "Moderna"

vacc_names <- data.frame(vaccine = c("Pfizer", "Oxford-AstraZeneca", "Moderna", "AstraZeneca Primary, Pfizer Booster", "AstraZeneca Primary, Moderna Booster"), vacc = c("PF", "AZ", "MD", "AZ-PF", "AZ-MD"))

vaccine_set <- vaccine
vacc_params <- readRDS("data/param_list.rds") %>%
  rename(vacc = vaccine) %>%
  left_join(vacc_names, by = "vacc") %>%
  filter(vaccine == vaccine_set) %>%
  mutate(std10 = 0.44) %>%
  filter(vfr > 1) %>%
  select(-c(vacc))

#### Set up other simulation parameters  ##############################################
target_pop <- 1e6
income_group <- "HIC"
hs_constraints <- "Absent"
dt <- 0.25
repetition <-  1:50
vacc_start <- "1/1/2021"
vaccine_doses <- c(3,6)
age_groups_covered <- 15
age_groups_covered_d4 <- c(5, 15)
seeding_cases <- 10
vacc_per_week <- 0.05
strategy <- "realistic"
t_d3 <- 227
t_d4 <- 365
vfr_time1 <- "11/27/2021"
vfr_time2 <- "12/31/2021"
vfr2_time1 <- "10/1/2022" # wont have any effect if vfr2 <- vfr, hosp_scale_vfr <- hosp_scale_vfr2 and ICU_scal_vfr <- ICU_scal_vfr2
vfr2_time2 <- "10/31/2022"
vfr <- sort(unique(vacc_params$vfr))[2]
vfr2 <- vfr
max_Rt_var2_scal <- 1
ICU_scal_vfr <-  0.3
hosp_scal_vfr <- 0.3
ICU_scal_vfr2 <- 0.3
hosp_scal_vfr2 <- 0.3
mu_ab_infection <- 1
mu_ab_inf_scal_vfr <- 0.5
max_ab <- 5
omicron_vaccine <- c(0,1)
vaccine_vfr <- 0.62*vfr
dose_4_fold_increase <- 1
vfr_drift_factor <- c(1.05, 1.1)
rt_drift_factor <- 1

#### Create scenarios ##########################################################

scenarios <- expand_grid(fit = fit,
                         income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         vaccine_doses = vaccine_doses,
                         vaccine = vaccine,
                         age_groups_covered = age_groups_covered,
                         age_groups_covered_d4 = age_groups_covered_d4,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         vacc_per_week = vacc_per_week,
                         t_d3 = t_d3,
                         t_d4 = t_d4,
                         vfr = vfr,
                         vfr2 = vfr2,
                         max_Rt_var2_scal = max_Rt_var2_scal,
                         vfr_time1 = vfr_time1,
                         vfr_time2 = vfr_time2,
                         vfr2_time1 = vfr2_time1,
                         vfr2_time2 = vfr2_time2,
                         mu_ab_infection = mu_ab_infection,
                         mu_ab_inf_scal_vfr = mu_ab_inf_scal_vfr,
                         max_ab = max_ab,
                         hosp_scal_vfr = hosp_scal_vfr,
                         ICU_scal_vfr = ICU_scal_vfr,
                         hosp_scal_vfr2 = hosp_scal_vfr2,
                         ICU_scal_vfr2 = ICU_scal_vfr2,  
                         omicron_vaccine = omicron_vaccine,
                         vaccine_vfr = vaccine_vfr,
                         dose_4_fold_increase = dose_4_fold_increase,
                         vfr_drift_factor = vfr_drift_factor,
                         rt_drift_factor = rt_drift_factor
) %>%
  mutate(age_groups_covered_d3 = age_groups_covered,
         age_groups_covered_d5 = age_groups_covered_d4,
         age_groups_covered_d6 = age_groups_covered_d4,
         age_groups_covered_d7 = age_groups_covered_d4,
         age_groups_covered_d8 = age_groups_covered_d4,
         age_groups_covered_d9 = age_groups_covered_d4 ) %>%
  mutate(t_d5 = if_else(vaccine_doses == 8, 181, 365), 
         t_d6 = if_else(t_d5 == 181, 184, 366), 
         t_d7 = if_else(t_d5 == 181, 182, 365),
         t_d8 = if_else(t_d5 == 181, 184, 365),
         t_d9 = 1) %>%
  filter((vaccine_doses==3 & age_groups_covered_d4 == 5) | (vaccine_doses == 6)) %>%
  filter(age_groups_covered_d4 <= age_groups_covered) %>%
  filter((vaccine_doses == 3 & omicron_vaccine == 0 )| vaccine_doses == 6 ) %>%
  mutate(rt_drift_factor = vfr_drift_factor) %>%
  unique()

scenarios$scenario <- 1:nrow(scenarios)
scenarios$name <- name
scenarios$strategy <- strategy

scenarios <- left_join(scenarios, vacc_params, by = c("vaccine", "vfr"))

nrow(scenarios)

write_csv(scenarios, paste0("scenarios/scenarios_", name, ".csv"))

## test on PC
source("R/run_function_main.R")
source("R/utils.R")
source("R/vaccine_strategy.R")
source("R/generate_rt_new.R")
source("R/generate_external_foi.R")
# plan(multicore, workers = 4)
# system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### Run the model on cluster ###############################################
# Load functions
sources <- c("R/run_function_main.R", "R/utils.R", "R/vaccine_strategy.R", "R/generate_rt_new.R", "R/generate_external_foi.R")
src <- conan::conan_sources(c("mrc-ide/safir", "mrc-ide/squire", "mrc-ide/nimue"))
ctx <- context::context_save("context",
                             sources = sources,
                             packages = c("tibble", "dplyr", "tidyr", "countrycode", "safir", "nimue", "squire", "data.table"),
                             package_sources = src)

config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--didemrchnb")

# Create the queue
run <- didehpc::queue_didehpc(ctx, config = config)

# Run
runs <- run$enqueue_bulk(scenarios, run_scenario, do_call = TRUE, progress = TRUE)
runs$status()

