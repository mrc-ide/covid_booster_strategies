library(squire)
library(nimue)
library(data.table)
library(parallel)
library(countrycode)
library(furrr)
library(zoo)
library(tibble)
library(wesanderson)
library(patchwork)
library(tidyverse)
library(purrr)
library(drjacoby)

source("R/vaccine_strategy.R")
source("R/plotting_utils.R")
source("R/utils.R")

library(safir)
#OR
library(safirimmunity)

name <- "test_new_model2"

max_t <- 720
rt <- 2
income_group <- "HIC"
target_pop <- 1e6
hs_constraints <- "Absent"
dt <- 0.25
seeding_cases <- 10
age_groups_covered <- 14
max_coverage <- 0.8
vacc_period <- 30
vacc_start <- 120
vaccine_doses <- 2
vfr <- 5.2
vfr2 <- 5.2
vfr_time1 <- 2
vfr_time2 <- 4
vfr2_time1 <- 30 # wont have any effect if vfr2 <- vfr, hosp_scale_vfr <- hosp_scale_vfr2 and ICU_scal_vfr <- ICU_scal_vfr2
vfr2_time2 <- 60

vaccine <- "vaccine"

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
t_d2 = 355
t_d3 = 181
t_d4 = 181
t_d5 = 181
t_d6 = 181
t_d7 = 181
t_d8 = 181
t_d9 = 181
immune_escape <- 1
matched_vacc <- 1
matched_vacc_level <- 1
max_ab <- 5

mu_ab_infection <- 1.13
std10_infection <- 0.44
mu_ab_inf_scal_vfr <- 1

scenario <- 1
age_groups_covered_d3 <- 14
age_groups_covered_d4 <- 14
age_groups_covered_d5 <- 14
repetition <- 1

ICU_scal_vfr <-  1
hosp_scal_vfr <- 1
ICU_scal_vfr2 <- 1
hosp_scal_vfr2 <- 1







