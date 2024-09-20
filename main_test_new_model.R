# Code to demonstrate how model outputs change with (a) new immunity model implementation; and (b) parameter changes to antibody decay
# Aim to is run the model to get endemic infections, then deploy one vaccine dose at 3 year mark

# install main packages
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

# source functions
source("R/vaccine_strategy.R")
source("R/plotting_utils.R")
source("R/utils.R")
source("R/immunity_process_old.R")
source("R/immunity_process_new.R")

# some plotting things
theme_set(theme_bw(base_size = 14))
th <- theme(strip.background = element_rect(fill = NA),
            panel.border = element_blank(),
            axis.line = element_line(),
            legend.text.align = 0,
            axis.text=element_text(size=16),
            axis.title=element_text(size=16),
            strip.text.x = element_text(size = 16))

# run 1: old immunity model (and old immunity parameter set)
detach("package:safirimmunity", unload = TRUE)
library(safir)

name <- "run1"
rt <- 5
source("R/test_params_old.R")

run_model_old()

# run 2: new immunity model (and old immunity parameter set)
detach("package:safir", unload = TRUE)
library(safirimmunity)
name <- "run2"
rt <- 5
source("R/test_params_old.R")

run_model_new()

# run 3: new immunity model (and new immunity parameter set)
name <- "run3"
rt <- 5
source("R/test_params_new.R")
run_model_new()

# need to increase Rt as prevalence is too low
name <- "run4"
rt <- 10
source("R/test_params_new.R")
run_model_new()

###########################################################################
# PLOT OUTPUTS #
###########################################################################
run1 <- readRDS("raw_outputs/output_run1/scenario_1.rds") %>%
  mutate(scenario = "run1")
run2 <- readRDS("raw_outputs/output_run2/scenario_1.rds") %>%
  mutate(scenario = "run2")

run3 <- readRDS("raw_outputs/output_run3/scenario_1.rds") %>%
  mutate(scenario = "run3")
run4 <- readRDS("raw_outputs/output_run4/scenario_1.rds") %>%
  mutate(scenario = "run4")

df <- rbind(run1, run2) %>%
  pivot_longer(cols = c(nat_ab_median, nat_median, vax_ab_median)) %>%
  mutate(name = factor(name, levels = c("nat_median", "nat_ab_median", "vax_ab_median"), labels = c("combined", "infection", "vaccine"))) %>%
  mutate(scenario = factor(scenario, labels = c("Additive immunity model (old)", "Maximum immunity model (new)")))

ggplot(data = df, aes(x = timestep, y = value, col = scenario, linetype = name)) +
  geom_line() + 
  labs(x = "timestep (days)", y = "immune level", linetype = "output type", color = "model") +
  scale_x_continuous(breaks = c(seq(0,365*5, by = 365))) +
  th

ggsave("compare_models_1.png", height = 5, width = 10)

df <- rbind(run1, run2) %>%
  mutate(scenario = factor(scenario, labels = c("Additive immunity model (old)", "Maximum immunity model (new)")))

ggplot(data = df, aes(x = timestep, y = rollmean(incidence,7, na.pad = TRUE), col = scenario)) +
  geom_line() + 
  labs(x = "timestep (days)", y = "incidence", color = "model") +
  scale_x_continuous(breaks = c(seq(0,365*5, by = 365)), limits = c(180, 1825)) +
  scale_y_continuous(limits = c(0,1250)) +
  th

ggsave("compare_models_2.png", height = 5, width = 10)

# can we plot population-level protection as well - against infection and severe disease?


# illustrate impact of new parameterisation (slower waning)
df2 <- rbind(run2, run3) %>%
  pivot_longer(cols = c(nat_ab_median, nat_median, vax_ab_median)) %>%
  mutate(name = factor(name, levels = c("nat_median", "nat_ab_median", "vax_ab_median"), labels = c("combined", "infection", "vaccine"))) %>%
  mutate(scenario = factor(scenario, labels = c("Old parameter set", "New parameter set")))

ggplot(data = df2, aes(x = timestep, y = value, col = scenario, linetype = name)) +
  geom_line() + 
  labs(x = "timestep (days)", y = "immune level", linetype = "output type", color = "model") +
  scale_x_continuous(breaks = c(seq(0,365*5, by = 365))) +
  th

ggsave("compare_models_3.png", height = 5, width = 10)


# illustrate impact of new parameterisation (slower waning) - higher Rt in new parameterisation though
df3 <- rbind(run2, run4) %>%
  pivot_longer(cols = c(nat_ab_median, nat_median, vax_ab_median)) %>%
  mutate(name = factor(name, levels = c("nat_median", "nat_ab_median", "vax_ab_median"), labels = c("combined", "infection", "vaccine"))) %>%
  mutate(scenario = factor(scenario, labels = c("Old parameter set", "New parameter set")))

ggplot(data = df3, aes(x = timestep, y = value, col = scenario, linetype = name)) +
  geom_line() + 
  labs(x = "timestep (days)", y = "immune level", linetype = "output type", color = "model") +
  scale_x_continuous(breaks = c(seq(0,365*5, by = 365))) +
  th

ggsave("compare_models_3_higherRt.png", height = 5, width = 10)

df3 <- rbind(run2, run4) %>%
  mutate(scenario = factor(scenario, labels = c("Old parameter set", "New parameter set")))

ggplot(data = df3, aes(x = timestep, y = rollmean(incidence,7, na.pad = TRUE), col = scenario)) +
  geom_line() + 
  labs(x = "timestep (days)", y = "incidence per 100,000", color = "model") +
  scale_x_continuous(breaks = c(seq(0,365*5, by = 365)), limits = c(180, 1825)) +
  scale_y_continuous(limits = c(0,1250)) +
  th

ggsave("compare_models_3_higherRt_inc.png", height = 5, width = 10)

# check prevalence of new parameterisation and model
z <- run4 %>%
  filter(timestep < max(run4$timestep),
         timestep > max(run4$timestep)-365)

av_infections <- z %>%
  summarise(infect = sum(incidence)/target_pop)
av_infections

prev <- z %>%
  mutate(prev = incidence /target_pop * 10 *100)
plot(prev$prev)

