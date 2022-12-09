get_external_foi_hic <- function(R0_t0,
                                 time_period){
  
  # these time points are used further down to define the start of the simulation and the small pulses that reduce stochasticity
  R0_t1 <- as.Date(x = "3/1/2020", format = "%m/%d/%Y")
  R0_t2 <- as.Date(x = "10/1/2020", format = "%m/%d/%Y")
  R0_t3 <- as.Date(x = "9/1/2021", format = "%m/%d/%Y")
  R0_t4 <- as.Date(x = "7/1/2022", format = "%m/%d/%Y")
  
  # daily per-capita prob of external infection
  lambda_external <- rep(0.0000001, time_period)
  
  # first pulse, spread out hazard of 0.001 over 10 days right before 1st wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t1, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  # second pulse, spread out hazard of 0.001 over 10 days right before 2nd wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t2, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  #  third pulse, spread out hazard of 0.001 over 20 days right before 3rd wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t3, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  return(lambda_external)
}

get_external_foi_lmic <- function(R0_t0,
                                  time_period){
  # these time points are used further down to define the start of the simulation and the small pulses that reduce stochasticity
  R0_t1 <- as.Date(x = "3/1/2020", format = "%m/%d/%Y")
  R0_t2 <- as.Date(x = "11/1/2020", format = "%m/%d/%Y")
  R0_t3 <- as.Date(x = "6/1/2021", format = "%m/%d/%Y")
  
  # daily per-capita prob of external infection
  lambda_external <- rep(1e-7, time_period)
  
  # first pulse, spread out hazard of 0.001 over 10 days right before 1st wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t1, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  # second pulse, spread out hazard of 0.001 over 10 days right before 2nd wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t2, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  #  third pulse, spread out hazard of 0.001 over 10 days right before 3rd wave
  t_spread <- 10
  lambda_tt <- as.integer(difftime(R0_t3, R0_t0 - 1))
  lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
  lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
  lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.001
  
  return(lambda_external)
}
