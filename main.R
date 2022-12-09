# Analysis of impact of COVID-19 booster doses on epidemic dynamics in different global settings, using the individual-based model of SARS-CoV-2 transmission, "safir"
# Authors: AB Hogan, SL Wu, AC Ghani, P Doohan, P Winskill, OJ Watson
# Date: 14 July 2022

#library(rstudioapi)
library(safir)
library(squire)
library(nimue)
library(data.table)
library(ggplot2)
library(parallel)
library(tidyverse)
library(countrycode)
library(furrr)
library(zoo)
library(tibble)
library(dplyr)
library(tidyr)
library(wesanderson)
library(patchwork)

source("R/plotting_utils.R")
source("R/utils.R")
source("R/generate_rt_new.R")
source("R/generate_external_foi.R")
source("3_postprocess/3_postprocess_hic_lmic.R")
source("3_postprocess/3_postprocess_lmic_rq2.R")
source("4_analysis/Figure3_S11_S12.R")

# get parameters
source("1_get_parameters/get_vaccine_params_main.R")

## model runs
# run from individual files in 2_model_runs

## postprocess
postprocess_hic_lmic(name = "rq1_hic_bv")
postprocess_hic_lmic(name = "rq1_hic")
postprocess_hic_lmic(name = "rq1_hic_newvariant")
postprocess_hic_lmic(name = "rq1_hic_drift")

postprocess_hic_lmic(name = "rq3_hic_bv")
postprocess_hic_lmic(name = "rq3_hic")
postprocess_hic_lmic(name = "rq3_hic_newvariant")
postprocess_hic_lmic(name = "rq3_hic_drift")

postprocess_hic_lmic(name = "rq4_lmic_bv")
postprocess_hic_lmic(name = "rq4_lmic_who")
postprocess_hic_lmic(name = "rq4_lmic")
postprocess_hic_lmic(name = "rq4_lmic_newvariant")
postprocess_hic_lmic(name = "rq4_lmic_drift")

postprocess_lmic_rq2(name = "rq2_lmic")

postprocess_hic_lmic(name = "rq1_hic_bv_test_inf_ab")

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
