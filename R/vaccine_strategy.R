get_vaccine_strategy <- function(name,
                                 strategy, 
                                 income_group,
                                 days_to_vacc_start, 
                                 doses_per_day, 
                                 time_period, 
                                 max_coverage, 
                                 age_groups_covered, 
                                 age_groups_covered_d3 = NA,
                                 age_groups_covered_d4 = NA,
                                 age_groups_covered_d5 = NA,
                                 age_groups_covered_d6 = NA,
                                 age_groups_covered_d7 = NA,
                                 age_groups_covered_d8 = NA,
                                 age_groups_covered_d9 = NA,
                                 vaccine_doses, 
                                 pop, 
                                 vacc_per_week, 
                                 t_d3 = NA, 
                                 t_d4 = NA,
                                 t_d5 = NA,
                                 t_d6 = NA,
                                 t_d7 = NA,
                                 t_d8 = NA,
                                 t_d9 = NA){
  
  cov_matrix <- read.csv(paste0("data/cov_", income_group, ".csv"), header = FALSE)
  
  if(name == "rq4_lmic_who"){  cov_matrix <- read.csv("data/cov_lmic_who_target.csv", header = FALSE)}
  
  colnames(cov_matrix) <- c("d1_d2", "d3")
  
  if (strategy == "realistic") {

    vaccine_set <- c(rep(0, days_to_vacc_start), rep(doses_per_day, time_period - days_to_vacc_start))
    vaccine_coverage_strategy <- list()
    
    if (vaccine_doses == 2) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
    } else if (vaccine_doses == 3) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*cov_matrix$d3)
    } else if (vaccine_doses ==4) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*cov_matrix$d3)
      vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d4,])*cov_matrix$d3)
    } else if (vaccine_doses ==5) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*cov_matrix$d3)
      vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d4,])*cov_matrix$d3)
      vaccine_coverage_strategy[[5]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d5,])*cov_matrix$d3)
    } else if (vaccine_doses ==6) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*cov_matrix$d3)
      vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d4,])*cov_matrix$d3)
      vaccine_coverage_strategy[[5]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d5,])*cov_matrix$d3)
      vaccine_coverage_strategy[[6]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d6,])*cov_matrix$d3)
    } else if (vaccine_doses ==7) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*cov_matrix$d3)
      vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d4,])*cov_matrix$d3)
      vaccine_coverage_strategy[[5]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d5,])*cov_matrix$d3)
      vaccine_coverage_strategy[[6]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d6,])*cov_matrix$d3)
      vaccine_coverage_strategy[[7]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d7,])*cov_matrix$d3)
    } else if (vaccine_doses ==8) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*cov_matrix$d3)
      vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d4,])*cov_matrix$d3)
      vaccine_coverage_strategy[[5]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d5,])*cov_matrix$d3)
      vaccine_coverage_strategy[[6]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d6,])*cov_matrix$d3)
      vaccine_coverage_strategy[[7]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d7,])*cov_matrix$d3)
      vaccine_coverage_strategy[[8]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d8,])*cov_matrix$d3)
    } else if (vaccine_doses ==9) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*cov_matrix$d3)
      vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d4,])*cov_matrix$d3)
      vaccine_coverage_strategy[[5]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d5,])*cov_matrix$d3)
      vaccine_coverage_strategy[[6]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d6,])*cov_matrix$d3)
      vaccine_coverage_strategy[[7]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d7,])*cov_matrix$d3)
      vaccine_coverage_strategy[[8]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d8,])*cov_matrix$d3)
      vaccine_coverage_strategy[[9]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d9,])*cov_matrix$d3)
    }
    
    
    next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
    
    if (vaccine_doses == 2) {
      next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
    } else if (vaccine_doses == 3) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[1,(17 - age_groups_covered + 1):17] <- 1
      next_dose_priority[2,(17 - age_groups_covered_d3 + 1):17] <- 1
    } else if (vaccine_doses == 4) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[1,(17 - age_groups_covered + 1):17] <- 1
      next_dose_priority[2,(17 - age_groups_covered_d3 + 1):17] <- 1
      next_dose_priority[3,(17 - age_groups_covered_d4 + 1):17] <- 1
    } else if (vaccine_doses == 5) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[1,(17 - age_groups_covered + 1):17] <- 1
      next_dose_priority[2,(17 - age_groups_covered_d3 + 1):17] <- 1
      next_dose_priority[3,(17 - age_groups_covered_d4 + 1):17] <- 1
      next_dose_priority[4,(17 - age_groups_covered_d5 + 1):17] <- 1
    } else if (vaccine_doses == 6) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[1,(17 - age_groups_covered + 1):17] <- 1
      next_dose_priority[2,(17 - age_groups_covered_d3 + 1):17] <- 1
      next_dose_priority[3,(17 - age_groups_covered_d4 + 1):17] <- 1
      next_dose_priority[4,(17 - age_groups_covered_d5 + 1):17] <- 1
      next_dose_priority[5,(17 - age_groups_covered_d6 + 1):17] <- 1
      
    } else if (vaccine_doses == 7) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[1,(17 - age_groups_covered + 1):17] <- 1
      next_dose_priority[2,(17 - age_groups_covered_d3 + 1):17] <- 1
      next_dose_priority[3,(17 - age_groups_covered_d4 + 1):17] <- 1
      next_dose_priority[4,(17 - age_groups_covered_d5 + 1):17] <- 1
      next_dose_priority[5,(17 - age_groups_covered_d6 + 1):17] <- 1
      next_dose_priority[6,(17 - age_groups_covered_d7 + 1):17] <- 1
      
    } else if (vaccine_doses == 8) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[1,(17 - age_groups_covered + 1):17] <- 1
      next_dose_priority[2,(17 - age_groups_covered_d3 + 1):17] <- 1
      next_dose_priority[3,(17 - age_groups_covered_d4 + 1):17] <- 1
      next_dose_priority[4,(17 - age_groups_covered_d5 + 1):17] <- 1
      next_dose_priority[5,(17 - age_groups_covered_d6 + 1):17] <- 1
      next_dose_priority[6,(17 - age_groups_covered_d7 + 1):17] <- 1
      next_dose_priority[7,(17 - age_groups_covered_d8 + 1):17] <- 1
      
    } else if (vaccine_doses == 9) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[1,(17 - age_groups_covered + 1):17] <- 1
      next_dose_priority[2,(17 - age_groups_covered_d3 + 1):17] <- 1
      next_dose_priority[3,(17 - age_groups_covered_d4 + 1):17] <- 1
      next_dose_priority[4,(17 - age_groups_covered_d5 + 1):17] <- 1
      next_dose_priority[5,(17 - age_groups_covered_d6 + 1):17] <- 1
      next_dose_priority[6,(17 - age_groups_covered_d7 + 1):17] <- 1
      next_dose_priority[7,(17 - age_groups_covered_d8 + 1):17] <- 1
      next_dose_priority[8,(17 - age_groups_covered_d9 + 1):17] <- 1
      
    }  
} 

  
  if (strategy == "same_doses"){
    
    
    population_coverage <- sum(pop$n[(17 - age_groups_covered + 1):17] * cov_matrix$d1_d2[(17 - age_groups_covered + 1):17])/sum(pop$n)
    days_to_vacc <- floor(population_coverage / (vacc_per_week/7) * 2)
    days_to_boost <- floor(days_to_vacc)
      
    if (days_to_vacc < 28) {days_to_vacc = 28}
    
    if (days_to_vacc + 28 >= (t_d3)) {t_d3 = days_to_vacc + 28}
      
      vaccine_set <- c(rep(0, days_to_vacc_start),
                       rep(doses_per_day/2, 28),
                       rep(doses_per_day, days_to_vacc - 28),
                       rep(doses_per_day/2, 28),
                       rep(0, t_d3 - days_to_vacc + 28),
                       rep(doses_per_day/2, days_to_boost),
                       rep(0, max(time_period - days_to_vacc_start - t_d3 - 56 -  days_to_boost,0)))
      
      vaccine_coverage_strategy <- list()
      
      if (vaccine_doses == 2) {
        vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:17,])*cov_matrix$d1_d2)
        vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:17,])*cov_matrix$d1_d2)
      } else if (vaccine_doses == 3) {
        vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
        vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
        vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)}
      
      if (vaccine_doses == 2) {
        next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
        next_dose_priority[,(17 - age_groups_covered + 1):17] <- 1
      } else if (vaccine_doses == 3) {
        next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
        next_dose_priority[,(17 - age_groups_covered + 1):17] <- 1
      } 
  }
  
  if (strategy == "two_doses_only") {
    population_coverage <- sum(pop$n[(17 - age_groups_covered + 1):17] * cov_matrix$d1_d2[(17 - age_groups_covered + 1):17])/sum(pop$n)
    days_to_vacc <- floor(population_coverage / (vacc_per_week/7) * 2)
    days_to_boost <- floor(days_to_vacc)
    
    if (days_to_vacc < 28) {days_to_vacc = 28}
    
    if (days_to_vacc + 28 >= (t_d3)) {t_d3 = days_to_vacc + 28}
    
    vaccine_set <- c(rep(0, days_to_vacc_start),
                     rep(doses_per_day/2, 28),
                     rep(doses_per_day, days_to_vacc - 28),
                     rep(doses_per_day/2, 28),
                     rep(0, max(time_period - days_to_vacc_start - days_to_vacc - 28, 0)))
    
    vaccine_coverage_strategy <- list()
    vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
    vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
    next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
    next_dose_priority[,(17 - age_groups_covered + 1):17] <- 1
    
  }

  if (strategy == "annual_boost") {
    population_coverage <- sum(pop$n[(17 - age_groups_covered + 1):17] * cov_matrix$d1_d2[(17 - age_groups_covered + 1):17])/sum(pop$n)
    days_to_vacc <- floor(population_coverage / (vacc_per_week/7) * 2)
    days_to_boost <- floor(days_to_vacc)
    
    if (days_to_vacc < 28) {days_to_vacc = 28}
    
    if (days_to_vacc + 28 >= (t_d3)) {t_d3 = days_to_vacc + 28}
    
    vaccine_set <- c(rep(0, days_to_vacc_start),
                     rep(doses_per_day/2, 28),
                     rep(doses_per_day, days_to_vacc - 28),
                     rep(doses_per_day/2, 28),
                     rep(0, t_d3 - days_to_vacc + 28),
                     rep(doses_per_day/2, days_to_boost),
                     rep(doses_per_day, max(time_period - days_to_vacc_start - t_d3 - 56 - days_to_boost,0)))
    
    vaccine_coverage_strategy <- list()
    
    vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
    vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
    vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)
    vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*cov_matrix$d1_d2)

    next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
    next_dose_priority[1,(17 - age_groups_covered + 1):17] <- 1
    next_dose_priority[2,(17 - age_groups_covered_d3 + 1):17] <- 1
    next_dose_priority[3,(17 - age_groups_covered_d4 + 1):17] <- 1 
  }
  
  return(
    list(
      vaccine_set = vaccine_set,
      vaccine_coverage_strategy = vaccine_coverage_strategy,
      next_dose_priority = next_dose_priority,
      t_d3 = t_d3,
      t_d4 = t_d4,
      t_d5 = t_d5,
      t_d6 = t_d6,
      t_d7 = t_d7,
      t_d8 = t_d8,
      t_d9 = t_d9
    )
  )
  
}
