
generate_Rt_hic <- function(max_Rt_var2_scal,
                            tmax_date,
                            vfr2_time1,
                            vfr2_time2,
                            name){
  # read in R profile
  R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")

  mult_v2 <- max_Rt_var2_scal

  vfr2_time1 <- as.Date(x = vfr2_time1, format = "%m/%d/%Y")
  vfr2_time2 <- as.Date(x = vfr2_time2, format = "%m/%d/%Y")
  
  tmax_date <- as.Date(x = tmax_date, format = "%m/%d/%Y")
  time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))
  
  R_profile <- read.csv("data/category_1_Rt.csv") 
  R_profile$date <- as.Date(R_profile$date, format = "%d/%m/%Y")
  
  dates <- R_profile$date
  rt <- R_profile$Rt
  rt_base_out <- safir::interpolate_rt(dates=dates, rt=rt, max_date=tmax_date)
  
  var2_dates <- c(R0_t0, vfr2_time1, vfr2_time2)
  rt_var2 <- c(1,1,mult_v2)
  rt_var2_mult <- safir::interpolate_rt(dates=var2_dates, rt=rt_var2, max_date=tmax_date)
  
  rt_out <- data.frame(Rt = rt_base_out$Rt * rt_var2_mult$Rt, Rt_tt = rt_base_out$Rt_tt)
  
  #saveRDS(rt_out, paste0("data/rt_out_hic_", name, ".rds"))
  
  return(rt_out)
}

generate_Rt_lmic <- function(max_Rt_var2_scal,
                             tmax_date,
                             vfr2_time1,
                             vfr2_time2,
                             name){
  # read in R profile
  R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")
  
  mult_v2 <- max_Rt_var2_scal
  vfr2_time1 <- as.Date(x = vfr2_time1, format = "%m/%d/%Y")
  vfr2_time2 <- as.Date(x = vfr2_time2, format = "%m/%d/%Y")
  
  tmax_date <- as.Date(x = tmax_date, format = "%m/%d/%Y")
  time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))
  
  R_profile <- read.csv("data/category_2_Rt.csv")
  R_profile$date <- as.Date(R_profile$date, format = "%d/%m/%Y")
  dates <- R_profile$date
  rt <- R_profile$Rt
  rt_base_out <- safir::interpolate_rt(dates=dates, rt=rt, max_date=tmax_date)
  
  var2_dates <- c(R0_t0, vfr2_time1, vfr2_time2)
  rt_var2 <- c(1,1,mult_v2)
  rt_var2_mult <- safir::interpolate_rt(dates=var2_dates, rt=rt_var2, max_date=tmax_date)
  
  rt_out <- data.frame(Rt = rt_base_out$Rt * rt_var2_mult$Rt, Rt_tt = rt_base_out$Rt_tt)
  
  return(rt_out)
}

generate_Rt_hic_zerocovid <- function(max_Rt_var2_scal,
                                      tmax_date,
                                      vfr2_time1,
                                      vfr2_time2,
                                      name) {
  
  R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")
  max_Rt <- 4
  mult_v2 <- max_Rt_var2_scal

  vfr2_time1 <- as.Date(x = vfr2_time1, format = "%m/%d/%Y")
  vfr2_time2 <- as.Date(x = vfr2_time2, format = "%m/%d/%Y")
  
  tmax_date <- as.Date(x = tmax_date, format = "%m/%d/%Y")
  time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))
  
  R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")
  R0_t1 <- as.Date(x = "3/1/2020", format = "%m/%d/%Y")
  R0_t2 <- as.Date(x = "5/1/2020", format = "%m/%d/%Y")
  R0_t3 <- as.Date("10/1/2021", format = "%m/%d/%Y")
  R0_t4 <- as.Date(R0_t3+365+90)   # gradual opening up
  
  dates <- c(R0_t0, R0_t1, R0_t2, R0_t3, R0_t4)
  rt <- c(1.1, 1.1, 0.2, 0.2, max_Rt)
  rt_base_out <- safir::interpolate_rt(dates=dates, rt=rt, max_date=tmax_date)
  
  var2_dates <- c(R0_t0, vfr2_time1, vfr2_time2)
  rt_var2 <- c(1,1,mult_v2)
  rt_var2_mult <- safir::interpolate_rt(dates=var2_dates, rt=rt_var2, max_date=tmax_date)
  
  rt_out <- data.frame(Rt = rt_base_out$Rt * rt_var2_mult$Rt, Rt_tt = rt_base_out$Rt_tt)
  
  return(rt_out)
  
}
