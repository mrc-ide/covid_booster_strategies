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

scenario = 1
target_pop = 1e6
income_group = "HIC"
hs_constraints = "Absent"
vacc_start = "1/1/2021"
vaccine_doses = 3

max_coverage = 0.8
age_groups_covered = 15
age_groups_covered_d3 = 2
age_groups_covered_d4 = 2
age_groups_covered_d5 = 2
age_groups_covered_d6 = 2
age_groups_covered_d7 = 2
age_groups_covered_d8 = 2
age_groups_covered_d9 = 2
dt = 0.25
repetition = 1
seeding_cases = 10
vfr = 5.1
vfr2 = 5.1
vfr_time1 <- "11/27/2021"
vfr_time2 <- "12/31/2021"
vfr2_time1 <- "10/1/2022" # wont have any effect if vfr2 <- vfr, hosp_scale_vfr <- hosp_scale_vfr2 and ICU_scal_vfr <- ICU_scal_vfr2
vfr2_time2 <- "10/31/2022"
max_Rt_var2_scal <- 1
ICU_scal_vfr <-  0.3
hosp_scal_vfr <- 0.3
ICU_scal_vfr2 <- 0.3
hosp_scal_vfr2 <- 0.3
mu_ab_infection <- 1
mu_ab_inf_scal_vfr <- 0.5
max_ab <- 5
omicron_vaccine <- 1
vaccine_vfr <- 3.162
dose_4_fold_increase <- 1
vfr_drift_factor <- 1.05
rt_drift_factor <- 1.05
max_Rt_var2_scal = 1
dose_3_fold_increase = 1
dose_4_fold_increase = 1
vacc_per_week <- 0.05
name = "rq1_hic_continual_drift"
std10 = 0.44
std10_infection = 0.44
t_d2 = 28
t_d3 = 227
t_d4 = 365
t_d5 = 365
t_d6 = 365
t_d7 = 365
t_d8 = 365
t_d9 = 181
mu_ab_d1 = 0.02
mu_ab_d2 = 0.5
k = 0.44
hl_s = 34
hl_l = 581
period_s = 60
period_l = 565
ab_50 = 0.03
ab_50_severe = 0.03
mu_ab_infection = 1
mu_ab_inf_scal_vfr = 1
max_ab = 5
strategy = "realistic"
hosp_scal_vfr = 1
ICU_scal_vfr = 1
hosp_scal_vfr2 = 1
ICU_scal_vfr2 = 1
variant_specific = 1
end_date = "12/31/2024"
vfr_drift_factor = 1.05
rt_drift_factor = 1.05

