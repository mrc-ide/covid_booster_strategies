# Analysis of impact of COVID-19 booster doses on epidemic dynamics in different global settings, using the individual-based model of SARS-CoV-2 transmission, "safir"
# Authors: AB Hogan, SL Wu, AC Ghani, P Doohan, P Winskill, OJ Watson, and others
# Date updated: June 2023

library(safir)
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

source("R/plotting_utils.R")
source("R/utils.R")
source("R/generate_rt_new.R")
source("R/generate_external_foi.R")
source("3_postprocess/3_postprocess_hic_lmic.R")
source("3_postprocess/3_postprocess_lmic_rq2.R")

# get parameters
source("1_get_parameters/get_vaccine_params_main.R")

## model runs
# run from individual files in 2_model_runs

## postprocess

postprocess_hic_lmic(name = "rq1_hic_bv_drift") # variant-adapted vaccine, 5% drift
postprocess_hic_lmic(name = "rq1_hic_newvariant") # new variant scenarios on top of 5% drift scenario
postprocess_hic_lmic(name = "rq1_hic_drift_sensitivity") # different drift scenarios (0%, 2.5%, 5%, 10%, 20%) on top of variant-adapted vaccine
postprocess_hic_lmic(name = "rq1_hic_drift") # ancestral vaccine, 5% drift
# postprocess_hic_lmic(name = "rq1_hic") # ancestral vaccine, no drift. exclude from analysis.
postprocess_hic_lmic(name = "rq1_hic_continual_drift") # continual strain-matching
postprocess_hic_lmic(name = "rq1_hic_bv") # variant-adapted vaccine but not drift



postprocess_hic_lmic(name = "rq3_hic_bv_drift") # variant-adapted vaccine, 5% drift
postprocess_hic_lmic(name = "rq3_hic_newvariant") # new variant scenarios on top of 5% drift scenario
postprocess_hic_lmic(name = "rq3_hic_drift_sensitivity") # different drift scenarios (0%, 2.5%, 5%, 10%, 20%) on top of variant-adapted vaccine
postprocess_hic_lmic(name = "rq3_hic_drift") # ancestral vaccine, 5% drift
postprocess_hic_lmic(name = "rq3_hic_continual_drift") # continual strain-matching
postprocess_hic_lmic(name = "rq3_hic_bv") # variant-adapted vaccine but not drift


postprocess_hic_lmic(name = "rq4_lmic_bv_drift") # variant-adapted vaccine, 5% drift
postprocess_hic_lmic(name = "rq4_lmic_newvariant") # new variant scenarios on top of 5% drift scenario
postprocess_hic_lmic(name = "rq4_lmic_drift_sensitivity") # different drift scenarios (0%, 2.5%, 5%, 10%, 20%) on top of variant-adapted vaccine
postprocess_hic_lmic(name = "rq4_lmic_who") # variant-adapted vaccine, 5% drift, WHO coverage scenario
postprocess_hic_lmic(name = "rq4_lmic_drift") # ancestral vaccine, 5% drift
postprocess_hic_lmic(name = "rq4_lmic_continual_drift") # continual strain-matching
postprocess_hic_lmic(name = "rq4_lmic_bv") # variant-adapted vaccine but not drift

#postprocess_lmic_rq2(name = "rq2_lmic") # ancestral vaccine, no drift, exclude from analysis
postprocess_lmic_rq2(name = "rq2_lmic_drift") # ancestral vaccine with 5% drift

postprocess_hic_lmic_mu_ab_inf(name = "rq1_hic_bv_test_inf_ab")
postprocess_hic_lmic_decayrate(name = "rq1_hic_decayrate")
### Generate figures

# Main figs

source("4_analysis/Figure1.R")
source("4_analysis/Figure2.R")
create_fig_s3_s11_s12(name = "rq1_hic_drift", filename = "Figure3")
source("4_analysis/Figure4.R") # additional VOC for category 1 and 2 settings

# Supp figs

# Figure S2 - Rt profiles
source("4_analysis/FigureS2.R")

# Figures additional results
source("4_analysis/FigureS5.R")
source("4_analysis/FigureS6.R")
source("4_analysis/FigureS7.R")
source("4_analysis/FigureS8.R") # LMIC comparison boost vs continue younger age groups
source("4_analysis/FigureS9.R") # WHO coverage sensitivity plot
source("4_analysis/FigureS10.R") # HIC bivalent vs current summary barplot
create_fig_s3_s11_s12(name = "rq4_lmic_drift", filename = "FigureS11") # variant drift scenarios LMIC
create_fig_s3_s11_s12(name = "rq3_hic_drift", filename = "FigureS12") # variant drift scenarios HIC cat 3
source("4_analysis/FigureS13.R") # additional VOC for category 3 setting
source("4_analysis/FigureS14.R") # sensitivity to mu_ab_infection parameter

### Generate tables
source("4_analysis/table_2_HIC.R")
source("4_analysis/table_2_LMIC.R")
source("4_analysis/table_2_LMIC_rq2.R")
