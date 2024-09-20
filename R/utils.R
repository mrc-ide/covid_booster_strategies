
add_cols <- function(x, df1){
  if (!x %in% names(df1)){
    n <- nrow(df1)
    df1[x] <- rep(0,n)
  } else {
    df1 <- df1
  }
  return(df1)
}

get_representative_country <- function(income_group){
  case_when(income_group == "HIC" ~ "Malta",
            income_group == "LMIC" ~ "Nicaragua")
}

get_representative_contacts <- function(income_group){
  case_when(income_group == "HIC" ~ "United Kingdom",
            income_group == "LMIC" ~ "Zimbabwe")
}

get_capacity <- function(country, income_group, pop, hs_constraints){
  hc <- squire::get_healthcare_capacity(country = country)
  
  # Unconstrained healthcare
  if(hs_constraints == "Absent"){
    hc$hosp_beds <- 1000000
    hc$ICU_beds <- 1000000
  }
  
  if(hs_constraints == "Present"){
    if(income_group %in% c("HIC", "UMIC")){
      hc$hosp_beds <- 1000000
      hc$ICU_beds <- 1000000
    }
    if(income_group %in% c("LMIC", "LIC")){
      hc$ICU_beds <- 0
    }
  }
  
  hc$hosp_beds <- round(hc$hosp_beds * sum(pop) / 1000)
  hc$ICU_beds <- round(hc$ICU_beds * sum(pop) / 1000)
  
  return(hc)
}

get_prob_non_severe_death_treatment <- function(income_group, hs_constraints){
  psdt <- squire:::probs$prob_non_severe_death_treatment
  
  if(income_group  == "LIC" & hs_constraints == "Present"){
    psdt <- c(rep(0.25, 16), 0.5804312)
  }
  return(psdt)
}

get_vaccine_pars <- function(
  income_group,
  mu_ab_d1,
  mu_ab_d2,
  vaccine_doses,
  ab_50,
  ab_50_severe,
  std10 = 0.44,
  k,
  t_d2,
  t_d3,
  t_d4,
  t_d5,
  t_d6,
  t_d7,
  t_d8,
  t_d9,
  hl_s,
  hl_l,
  period_s,
  period_l,
  max_ab = 5,
  vfr2,
  immune_escape = 0,
  matched_vacc = 0,
  matched_vacc_level = 0
){
  # if implementing bivalent vaccine, start from 4th dose by reversing out impact of vfr (but retaining higher immunogenicity of 4th dose)
  
  if (immune_escape == 1){
    if (matched_vacc == 1){
      mu_ab_d1 <- mu_ab_d1 * matched_vacc_level * vfr2
      mu_ab_d2 <- mu_ab_d2 * matched_vacc_level * vfr2
    } 
  }
   
  mu_ab_list <- data.frame(name = "Moderna",
                           mu_ab_d1 = mu_ab_d1,
                           mu_ab_d2 = mu_ab_d2) %>%
    mutate(mu_ab_d3 = mu_ab_d2) %>%
    mutate(mu_ab_d4 = mu_ab_d3) %>%
    mutate(mu_ab_d5 = mu_ab_d4) %>%
    mutate(mu_ab_d6 = mu_ab_d4) %>%
    mutate(mu_ab_d7 = mu_ab_d4) %>%
    mutate(mu_ab_d8 = mu_ab_d4) %>%
    mutate(mu_ab_d9 = mu_ab_d4)
    
  
  if(is.na(mu_ab_list$mu_ab_d6)){mu_ab_list$mu_ab_d6 <- mu_ab_list$mu_ab_d5}
  if(is.na(mu_ab_list$mu_ab_d7)){mu_ab_list$mu_ab_d7 <- mu_ab_list$mu_ab_d6}
  if(is.na(mu_ab_list$mu_ab_d8)){mu_ab_list$mu_ab_d8 <- mu_ab_list$mu_ab_d7}
  if(is.na(mu_ab_list$mu_ab_d9)){mu_ab_list$mu_ab_d9 <- mu_ab_list$mu_ab_d8}
  
  
  ab_parameters <- safir::get_vaccine_ab_titre_parameters(
    vaccine = "Moderna",
    max_dose = vaccine_doses,
    correlated = FALSE,
    hl_s = hl_s,
    hl_l = hl_l,
    period_s = period_s,
    t_period_l = period_l,
    ab_50 = ab_50,
    ab_50_severe = ab_50_severe,
    std10 = std10,
    k = k,
    mu_ab_list = mu_ab_list
  )
  
  ab_parameters$max_ab <- max_ab # max titre on natural log scale
  return(ab_parameters) 
}
