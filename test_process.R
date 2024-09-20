x <- readRDS("raw_outputs/output_test_old_model/scenario_1.rds")
head(x)
plot(x$nat_ab_mean)
plot(x$vax_ab_mean)

plot(x$nat_mean) # this is the combined value of titres
lines(x$nat_ab_mean)
lines(x$vax_ab_mean, col = "red")
lines(x$vax_ab_mean + x$nat_ab_mean, col = "purple", lwd = 3) # the combined titre is equal to the sum of vaccine and natural antibodies

#plot(x$sp_mean)



# hm this doesnt really make sense - as it is at the population level, whereas we want to take maximum at individual level

y <- readRDS("raw_outputs/output_test_new_model/scenario_1.rds")
head(y)
plot(y$nat_ab_mean)
plot(y$vax_ab_mean)
plot(y$nat_mean)
lines(y$nat_ab_mean)
lines(y$vax_ab_mean, col = "red")
lines(y$vax_ab_mean + y$nat_ab_mean, col = "purple", lwd = 2)

# why is the titre so high in the updated model
plot(x$incidence)
lines(y$incidence)

plot(x$X1_count)
lines(y$X1_count)


# models are now starting to work...
# model with "old" paramterisation and old decay rate vector,
# old immunity model
# 2 vaccine doses
z <- readRDS("raw_outputs/output_test_new_model5/scenario_1.rds")
head(z)
plot(z$nat_mean)
lines(z$nat_ab_mean)
lines(z$vax_ab_mean)

plot(z$incidence)
plot(z$incidence[100:1439])

# total number of infections per person per year, in first year following vaccine, and average prevalence (assuming 10-day infected period)
target_pop <- 2e5
df <- z %>%
  filter(timestep < max(z$timestep),
         timestep > max(z$timestep)-365)

av_infections <- df %>%
  summarise(infect = sum(incidence)/target_pop)

prev <- df %>%
  mutate(prev = incidence /target_pop * 10 *100)
plot(prev$prev)
  
av_infections

# model with old immunity model, 3 doses vaccine
z_3dose <- readRDS("raw_outputs/output_test_new_model6/scenario_1.rds")
plot(z$nat_mean)
lines(z$nat_ab_mean)
lines(z$vax_ab_mean)
lines(z_3dose$nat_mean, col = "darkred")
lines(z_3dose$nat_ab_mean, col = "purple")
lines(z_3dose$vax_ab_mean, col = "red")


plot(z$incidence[100:1439])
lines(z_3dose$incidence[100:1439])


# model with new immunity model implemented, 3 doses vaccine
z_newmodel <- readRDS("raw_outputs/output_test_new_model7/scenario_1.rds")
plot(z$nat_mean)
lines(z$nat_ab_mean)
lines(z$vax_ab_mean)
lines(z_3dose$nat_mean, col = "darkred")
lines(z_3dose$nat_ab_mean, col = "purple")
lines(z_3dose$vax_ab_mean, col = "red")

lines(z_newmodel$nat_mean, col = "darkgreen")
lines(z_newmodel$nat_ab_mean, col = "green")
lines(z_newmodel$vax_ab_mean, col = "lightgreen")

plot(z$incidence[1:1439])
lines(z$incidence[1:1439])

lines(z_3dose$incidence[1:1439], col = "red")
lines(z_newmodel$incidence[1:1439], col = "purple")

target_pop <- 2e5
df <- z_newmodel %>%
  filter(timestep < max(z$timestep),
         timestep > max(z$timestep)-365)
av_infections <- df %>%
  summarise(infect = sum(incidence)/target_pop)

prev <- df %>%
  mutate(prev = incidence /target_pop * 10 *100)
plot(prev$prev)

# now see what difference updating to new parameter set makes
z_newparms <- readRDS("raw_outputs/output_test_new_model8/scenario_1.rds")
plot(z_newmodel$nat_mean)
lines(z_newparms$nat_mean)

plot(z_newmodel$incidence[1:1439])
lines(z_newparms$incidence[1:1439])

# hmm the decay was not scaled correctly. now fixed
z_newparms2 <- readRDS("raw_outputs/output_test_new_model9/scenario_1.rds")
plot(z_newmodel$nat_mean)
lines(z_newparms$nat_mean)
lines(z_newparms2$nat_mean, col = "red")

plot(z_newmodel$incidence[1:1439])
lines(z_newparms$incidence[1:1439])
lines(z_newparms2$incidence[1:1439], col = "red")

target_pop <- 2e5
df <- z_newparms2 %>%
  filter(timestep < max(z_newparms2$timestep),
         timestep > max(z_newparms2$timestep)-365)
av_infections <- df %>%
  summarise(infect = sum(incidence)/target_pop)

prev <- df %>%
  mutate(prev = incidence /target_pop * 10 *100)
plot(prev$prev)

# prevalence a little low (not enough infections)
# try increase Rt to 7
z_newparms3 <- readRDS("raw_outputs/output_test_new_model10/scenario_1.rds")
plot(z_newmodel$nat_mean)
lines(z_newparms$nat_mean)
lines(z_newparms2$nat_mean, col = "red")
lines(z_newparms3$nat_mean, col = "purple")

plot(z_newmodel$incidence[1:1439])
lines(z_newparms$incidence[1:1439])
lines(z_newparms2$incidence[1:1439], col = "red")
lines(z_newparms3$incidence[1:1439], col = "purple")

target_pop <- 2e5
df <- z_newparms3 %>%
  filter(timestep < max(z_newparms3$timestep),
         timestep > max(z_newparms3$timestep)-365)
av_infections <- df %>%
  summarise(infect = sum(incidence)/target_pop)
av_infections
prev <- df %>%
  mutate(prev = incidence /target_pop * 10 *100)
plot(prev$prev)

# prevalence a little low (not enough infections)
# try increase Rt to 8
z_newparms4 <- readRDS("raw_outputs/output_test_new_model11/scenario_1.rds")
plot(z_newmodel$nat_mean)
lines(z_newparms$nat_mean)
lines(z_newparms2$nat_mean, col = "red")
lines(z_newparms3$nat_mean, col = "purple")
lines(z_newparms4$nat_mean, col = "darkgreen")

plot(z_newmodel$incidence[1:1439])
lines(z_newparms$incidence[1:1439])
lines(z_newparms2$incidence[1:1439], col = "red")
lines(z_newparms3$incidence[1:1439], col = "purple")

lines(z_newparms4$incidence[1:1439], col = "purple")

target_pop <- 2e5
df <- z_newparms3 %>%
  filter(timestep < max(z_newparms3$timestep),
         timestep > max(z_newparms3$timestep)-365)
av_infections <- df %>%
  summarise(infect = sum(incidence)/target_pop)
av_infections

prev <- df %>%
  mutate(prev = incidence /target_pop * 10 *100)
plot(prev$prev)

# prevalence a little low (not enough infections)
# try increase Rt to 8
z1 <- readRDS("raw_outputs/output_test_new_model1a/scenario_1.rds")
plot(z1$nat_mean)
lines(z1$nat_ab_mean)
lines(z1$vax_ab_mean)
lines(z_newparms2$nat_mean, col = "red")
lines(z_newparms3$nat_mean, col = "purple")
lines(z_newparms4$nat_mean, col = "darkgreen")

plot(z_newmodel$incidence[1:1439])
lines(z_newparms$incidence[1:1439])
lines(z_newparms2$incidence[1:1439], col = "red")
lines(z_newparms3$incidence[1:1439], col = "purple")

lines(z_newparms4$incidence[1:1439], col = "purple")

target_pop <- 2e5
df <- z_newparms3 %>%
  filter(timestep < max(z_newparms3$timestep),
         timestep > max(z_newparms3$timestep)-365)
av_infections <- df %>%
  summarise(infect = sum(incidence)/target_pop)
av_infections

prev <- df %>%
  mutate(prev = incidence /target_pop * 10 *100)
plot(prev$prev)

plot(z1$X1_count)
