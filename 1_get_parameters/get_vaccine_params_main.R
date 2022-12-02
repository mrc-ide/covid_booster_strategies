library(tidyverse)
library(patchwork)
library(dplyr)
library(purrr)
library(drjacoby)

##################################
##### LOAD THE PARAMETERS 
##################################

load("data_chains/UKHSA_v6_65+_20220702_AZPD2=FALSE_SB=FALSE_NewDecay=TRUE_AddBst=FALSE_AltSev=FALSE_mcmc_chain.Rdata")  

###################################
## Calculate parameter estimates and bounds of transformed parameters from 10,000 MCMC samples
###################################

draws <- sample_chains(mcmc, 10000)

draws_transform <- draws %>%
  select(-sample, -AZ_ns_off ) %>%
  mutate( ab50 = 10^(d2_PF + ni50),
          ab50_s = 10^(d2_PF + ns50), 
          ab50_d = 10^(d2_PF + nd50),
          d1_AZ = 10^(d2_AZ + d1_AZ),
          d1_PF = 10^(d2_PF + d1_PF),
          d1_MD = 10^(d2_MD + d1_MD),
          d2_PF = 10^(d2_PF),
          d2_AZ = 10^(d2_AZ),
          d2_MD = 10^(d2_MD),
          d3_PF = 10^(bst_PF),
          d3_AZ = 10^(bst_AZ),
          d3_MD = 10^(bst_MD),
          om_red = 10^(om_red)) %>%
  select(-ni50, -ns50, -nd50, -bst_PF, -bst_AZ, -bst_MD) 

posterior_median <-draws_transform %>%
  summarise( 
    across(where(is.numeric), median)
  )%>%
  mutate(measure = "median")

posterior_upper <- draws_transform %>%
  summarise( 
    across(where(is.numeric), quantile, 0.975)
  )%>%
  mutate(measure = "upper")

posterior_lower <- draws_transform %>%
  summarise( 
    across(where(is.numeric), quantile, 0.025)
  ) %>%
  mutate(measure = "lower")

#####################################################
### calculate decay rate vector
######################################################

hl_s <- posterior_median$hl_s
hl_l <- posterior_median$hl_l
period_s <- posterior_median$period_s

max_t     <- 365*10
t         <- 0:(max_t-1) #vaccinated on day 0
dr_s      <- -log(2)/hl_s # Corresponding decay rate in days for half life above
dr_l      <- -log(2)/hl_l

# simple biphasic decay implemented as sum of decaying exponentials
denom=log(exp(dr_l*period_s)+exp(dr_s*period_s))
cum_dr_vec=log(exp(dr_s*t+dr_l*period_s)+exp(dr_l*t+dr_s*period_s))-denom
dr_vec=c(0,diff(cum_dr_vec,1))

D1 <- dr_vec
D2 <- dr_vec
D3 <- dr_vec
D4 <- dr_vec
D5 <- dr_vec
D6 <- dr_vec
D7 <- dr_vec
D8 <- dr_vec
D9 <- dr_vec

# natural immunity is the same

N <- D1

# output to data frame for reading in

dr_vec_new <- data.frame(t,D1,D2,D3,D4,D5,D6, D7, D8, D9, N)

saveRDS(dr_vec_new, "data/dr_vec.rds")

#################################
### save parameter list for runs
#################################

pm <- posterior_median
vaccine <- c("AZ", "PF", "MD", "AZ-PF", "AZ-MD")
vfr <- c(1,round(posterior_median$om_red, 1), round(posterior_lower$om_red,1),round(posterior_upper$om_red,1))

mu_ab_d1 <- c(pm$d1_AZ, pm$d1_PF, pm$d1_MD, pm$d1_AZ, pm$d1_AZ)
mu_ab_d2 <- c(pm$d2_AZ, pm$d2_PF, pm$d2_MD, pm$d2_AZ, pm$d2_AZ)
mu_ab_d3 <- c(pm$d3_AZ, pm$d3_PF, pm$d3_MD, pm$d3_PF, pm$d3_MD)
k <- pm$k
hl_s <- pm$hl_s
hl_l <- pm$hl_l
period_s <- pm$period_s
period_l <- pm$period_l
dose_3_fold_increase <- mu_ab_d3/mu_ab_d2
ab_50 <- pm$ab50
ab_50_severe <- pm$ab50_s

param_list <- data.frame(vaccine,mu_ab_d1,mu_ab_d2, k, dose_3_fold_increase, hl_s, hl_l, period_s, period_l, ab_50, ab_50_severe) 

param_list_out <- 
  # Create input options
  expand_grid(
    vfr = vfr,
    vaccine = c("AZ", "PF", "MD", "AZ-PF", "AZ-MD")) %>%
  # Join with MCMC samples
  left_join(param_list, by = "vaccine") 

saveRDS(param_list_out, "data/param_list.rds")

