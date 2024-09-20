get_vaccine_strategy <- function(income_group,
                                 vacc_start, 
                                 doses_per_day, 
                                 time_period, 
                                 max_coverage, 
                                 age_groups_covered, 
                                 age_groups_covered_d3 = NA,
                                 age_groups_covered_d4 = NA,
                                 age_groups_covered_d5 = NA,
                                 vaccine_doses, 
                                 pop){

    vaccine_set <- c(rep(0, vacc_start), rep(doses_per_day, time_period - vacc_start))
    vaccine_coverage_strategy <- list()
    
    if (vaccine_doses == 1) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
    } else if (vaccine_doses == 2) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
    } else if (vaccine_doses == 3) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*max_coverage)
    } else if (vaccine_doses ==4) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*max_coverage)
      vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d4,])*max_coverage)
    } else if (vaccine_doses ==5) {
      vaccine_coverage_strategy[[1]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
      vaccine_coverage_strategy[[2]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered,])*max_coverage)
      vaccine_coverage_strategy[[3]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d3,])*max_coverage)
      vaccine_coverage_strategy[[4]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d4,])*max_coverage)
      vaccine_coverage_strategy[[5]] <- t(t(nimue::strategy_matrix(strategy = "Elderly",max_coverage = 1)[1:age_groups_covered_d5,])*max_coverage)
    }
    
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
    } 

    if (vaccine_doses >= 2) {
      out <-     list(
        vaccine_set = vaccine_set,
        vaccine_coverage_strategy = vaccine_coverage_strategy,
        next_dose_priority = next_dose_priority)
    } else if (vaccine_doses == 1) {
      out <-     list(
        vaccine_set = vaccine_set,
        vaccine_coverage_strategy = vaccine_coverage_strategy)
    }

  return(out)
  
}
