# main running function
run_model_old <- 
  function(){
    
    # set up timing and transmission
    time_period <- max_t
    
    # transmission vector
    rt_out <- rep(rt, time_period)
    
    # set up daily per-capita prob of external infection with external pulse
    lambda_external <- rep(1e-6, time_period)
    
    # BASE PARAMETERS
    
    # Population and mixing
    rep_country <- get_representative_country(income_group = income_group)
    iso3c <- countrycode(rep_country, origin = "country.name", destination = "iso3c")
    pop <- squire::get_population(country = rep_country)
    pop_standardise <- target_pop / sum(pop$n)
    pop$n <- as.integer(pop$n * pop_standardise)
    
    contact_country <- get_representative_contacts(income_group = income_group)
    contact_mat <- squire::get_mixing_matrix(country = contact_country)
    
    # Hospital capacity
    hc <- get_capacity(country = rep_country, income_group = income_group, pop = pop$n, hs_constraints = hs_constraints)
    
    # Poorer health outcomes for LMICs and LICs
    pnsdt = get_prob_non_severe_death_treatment(income_group, hs_constraints) 
    
    # base parameters
    parameters <- safir::get_parameters(
      population = pop$n,
      contact_matrix_set = contact_mat,
      iso3c = iso3c,
      R0 = rt_out,
      tt_R0 = c(1:time_period),
      time_period = time_period,
      dt = dt,
      hosp_bed_capacity = hc$hosp_beds,
      ICU_bed_capacity = hc$ICU_beds,
      prob_non_severe_death_treatment = pnsdt,
      seeding_cases = seeding_cases,
      lambda_external = lambda_external[1:time_period]
    )
    
    # --------------------------------------------------------------------------------
    # get vaccine parameters
    # --------------------------------------------------------------------------------
    # doses available each day
    # get from nimue
    
    population_coverage <- max_coverage * sum(pop$n[(17 - age_groups_covered + 1):17])
    doses_per_day <- floor(population_coverage / vacc_period)
    
    
    vaccine_out <-
      get_vaccine_strategy(
        income_group,
        vacc_start = vacc_start,
        doses_per_day = doses_per_day,
        time_period = time_period,
        max_coverage = max_coverage,
        age_groups_covered = age_groups_covered,
        age_groups_covered_d3 = age_groups_covered_d3,
        age_groups_covered_d4 = age_groups_covered_d4,
        age_groups_covered_d5 = age_groups_covered_d5,
        vaccine_doses = vaccine_doses,
        pop = pop
      )
    
    
    vaccine_set <- vaccine_out$vaccine_set
    vaccine_coverage_strategy <- vaccine_out$vaccine_coverage_strategy
    if (vaccine_doses >= 2) {
      next_dose_priority <- vaccine_out$next_dose_priority
    } else {
      next_dose_priority <- NULL
    }
    
    # dosing
    if (vaccine_doses == 1) {dose_period <- c(1)}
    if (vaccine_doses == 2) {dose_period <- c(NaN, 28)}
    if (vaccine_doses == 3) {dose_period <- c(NaN, 28, t_d3)}
    if (vaccine_doses == 4) {dose_period <- c(NaN, 28, t_d3, t_d4)}
    if (vaccine_doses == 5) {dose_period <- c(NaN, 28, t_d3, t_d4, t_d5)}
    
    # make VFR reduction vector and attach
    vfr_vector <- c(rep(1, (vfr_time1 - 1)),
                    seq(1, vfr, length = vfr_time2 - vfr_time1 + 1),
                    rep(vfr, (vfr2_time1 - vfr_time2)),
                    seq(vfr, vfr2, length = vfr2_time2 - vfr2_time1 + 1),
                    rep(vfr2, time_period - vfr2_time2-1))
    
    vfr_final <- vfr_vector 
    
    # profiles
    vax_pars <- get_vaccine_pars(income_group = income_group,
                                 mu_ab_d1 = mu_ab_d1,
                                 mu_ab_d2 = mu_ab_d2,
                                 vaccine_doses = vaccine_doses,
                                 ab_50 = ab_50,
                                 ab_50_severe = ab_50_severe,
                                 std10 = std10,
                                 k = k,
                                 t_d2 = t_d2,
                                 t_d3 = t_d3,
                                 t_d4 = t_d4,
                                 t_d5 = t_d5,
                                 t_d6 = t_d6,
                                 t_d7 = t_d7,
                                 t_d8 = t_d8,
                                 t_d9 = t_d9,
                                 hl_s = hl_s,
                                 hl_l = hl_l,
                                 period_s = period_s,
                                 period_l = period_l,
                                 max_ab = max_ab,
                                 vfr2 = vfr2,
                                 immune_escape = immune_escape,
                                 matched_vacc = matched_vacc,
                                 matched_vacc_level = matched_vacc_level)
    
    
    
    # combine parameters and verify
    parameters <- make_vaccine_parameters(
      safir_parameters = parameters,
      vaccine_ab_parameters = vax_pars,
      vaccine_set = vaccine_set,
      dose_period = dose_period,
      strategy_matrix = vaccine_coverage_strategy,
      next_dose_priority_matrix = next_dose_priority
    )
    
    # set up mu_ab_infection vector
    mu_ab_infection_vector <- c(rep(mu_ab_infection, (vfr_time1 - 1)),
                                seq(mu_ab_infection, mu_ab_infection*vfr, length = vfr_time2 - vfr_time1 + 1),
                                rep(mu_ab_infection*vfr, (vfr2_time1 - vfr_time2)),
                                seq(mu_ab_infection*vfr, mu_ab_infection*vfr2, length = vfr2_time2 - vfr2_time1 + 1),
                                rep(mu_ab_infection*vfr2, time_period - vfr2_time2-1))
    
    # apply the potential reduction in protection against reinfection again each time a new variant emerges
    
    mu_ab_inf_scal_vfr_vector <- c(rep(1, (vfr_time1 - 1)),
                                   seq(1, mu_ab_inf_scal_vfr, length = vfr_time2 - vfr_time1 + 1),
                                   rep(mu_ab_inf_scal_vfr, time_period - vfr_time2 ))
    
    mu_ab_infection_vector_in <- t(as.matrix(mu_ab_infection_vector * mu_ab_inf_scal_vfr_vector))
    
    
    parameters <-
      make_immune_parameters(
        parameters = parameters,
        vfr = vfr_vector,
        mu_ab_infection = mu_ab_infection_vector_in,
        std10_infection = std10_infection
      )
    
    # apply modification to rate of hospitalisation and ICU
    
    v <- parameters$prob_hosp
    parameters$prob_hosp <-
      matrix(v,
             nrow = time_period,
             ncol = length(v),
             byrow = TRUE)
    mult <-
      c(
        rep(1, vfr_time1 - 1),
        seq(1, hosp_scal_vfr, length.out = vfr_time2 - vfr_time1 + 1),
        rep(hosp_scal_vfr, (vfr2_time1 - vfr_time2)),
        seq(hosp_scal_vfr, hosp_scal_vfr2, length = vfr2_time2 - vfr2_time1 + 1),
        rep(hosp_scal_vfr2, time_period - vfr2_time2-1)
      )
    new <- parameters$prob_hosp * mult
    parameters$prob_hosp <- t(new)
    
    v <- parameters$prob_severe
    parameters$prob_severe <-
      matrix(v,
             nrow = time_period,
             ncol = length(v),
             byrow = TRUE)
    mult <-
      c(
        rep(1, vfr_time1 - 1),
        seq(1, ICU_scal_vfr, length.out = vfr_time2 - vfr_time1 + 1),
        rep(ICU_scal_vfr, (vfr2_time1 - vfr_time2)),
        seq(ICU_scal_vfr, ICU_scal_vfr2, length = vfr2_time2 - vfr2_time1 + 1),
        rep(ICU_scal_vfr2, time_period - vfr2_time2-1)
      )
    new <- parameters$prob_severe * mult
    parameters$prob_severe <- t(new)
    
    
    # decay rate vector: biphasic implementation rather than switching point
    dr_s <- -log(2)/hl_s
    dr_l <- -log(2)/hl_l
    numerator <- log10(exp(dr_s * c(1:(max_t+1)) + dr_l * period_s) + exp(dr_l * c(1:(max_t+1)) + dr_s *
                                                                            period_s))
    denominator <- log10(exp(dr_l * period_s) + exp(dr_s * period_s))
    cum_dr_vec <-  numerator - denominator
    
    dr_vec <- diff(cum_dr_vec)*log(10)
    dr_vec_vaccine <- data.frame(d1 = dr_vec, d2 = dr_vec, d3 = dr_vec, d4 = dr_vec, d5 = dr_vec)
    
    # assume that the decay rate for natural infection is the same as for the vaccine
    dr_vec_inf <- dr_vec
    
    dr_vec_doses_m <- data.matrix(dr_vec_vaccine)
    dr_vec_inf_m <- data.matrix(dr_vec_inf)
    parameters <-
      make_independent_vaccine_infection_nat_parameters(
        parameters = parameters,
        dr_vec_doses = dr_vec_doses_m,
        dr_vec_inf = dr_vec_inf_m,
        max_ab_inf = max_ab
      )
    
    ######################################################
    # run the simulation
    ######################################################
    
    # create variables
    timesteps <- parameters$time_period/dt
    
    # creates the categorical states and ages for the simulated population
    variables <- create_variables(pop = pop, parameters = parameters)
    variables <- create_vaccine_variables(variables = variables, parameters = parameters)
    variables <- create_natural_immunity_variables(variables = variables, parameters = parameters)
    variables <- create_independent_nat_variables(variables = variables, parameters = parameters)
    
    # creates the list of events and attaches listeners which handle state changes and queue future events
    events <- create_events(parameters = parameters)
    events <- create_events_vaccination(events = events, parameters = parameters)
    attach_event_listeners(variables = variables, events = events, parameters = parameters, dt = dt)
    attach_event_listeners_vaccination(variables = variables,events = events,parameters = parameters,dt = dt)
    # attach_event_listeners_natural_immunity(variables = variables, events = events, parameters = parameters, dt = dt, additive = TRUE)
    attach_event_listeners_independent_nat(variables = variables, events = events, parameters = parameters, dt = dt)
    
    # renderer object is made
    renderer <- individual::Render$new(parameters$time_period)
    vaxx_renderer <- individual::Render$new(parameters$time_period)
    inf_renderer <- individual::Render$new(parameters$time_period)
    incidence_renderer <- individual::Render$new(timesteps)
    attach_tracking_listener_incidence(events=events, renderer = incidence_renderer)
    nat_renderer <- individual::Render$new(parameters$time_period)
    nat_inf_renderer <- individual::Render$new(parameters$time_period)
    sp_renderer <- individual::Render$new(parameters$time_period)
    hosp_render <- create_hosp_renderers(parameters = parameters)
    
    # track incidence
    attach_hosp_listeners(renderers = hosp_render, events = events)
    
    double_count_render_process_daily <- function(renderer, variable, dt) {
      stopifnot(inherits(variable, "DoubleVariable"))
      stopifnot(inherits(renderer, "Render"))
      function(t) {
        if ((t * dt) %% 1 == 0) {
          day <- as.integer(t * dt)
          nat <- exp(variable$get_values())
          quantiles_nat <- quantile(x = nat, probs = c(0.025, 0.5, 0.975))
          renderer$render(name = "lower", value = quantiles_nat[[1]], timestep = day)
          renderer$render(name = "median", value = quantiles_nat[[2]], timestep = day)
          renderer$render(name = "upper", value = quantiles_nat[[3]], timestep = day)
          renderer$render(name = "mean", value = mean(x = nat), timestep = day)
        }
      }
    }
    
    sp_render_process_daily <- function(renderer, variable1, variable2, dt) {
      stopifnot(inherits(variable1, "DoubleVariable"))
      stopifnot(inherits(variable2, "DoubleVariable"))
      stopifnot(inherits(renderer, "Render"))
      function(t) {
        if ((t * dt) %% 1 == 0) {
          day <- as.integer(t * dt)
          nat <- exp(variable1$get_values()) + exp(variable2$get_values())
          quantiles <- quantile(x = nat, probs = c(0.025, 0.5, 0.975))
          renderer$render(name = "nat_lower", value = quantiles[[1]], timestep = day)
          renderer$render(name = "nat_median", value = quantiles[[2]], timestep = day)
          renderer$render(name = "nat_upper", value = quantiles[[3]], timestep = day)
          renderer$render(name = "nat_mean", value = mean(x = nat), timestep = day)
          sp <-ifelse(nat>=0.2,1,0)
          quantiles <- quantile(x = sp, probs = c(0.025, 0.5, 0.975))
          renderer$render(name = "sp_lower", value = quantiles[[1]], timestep = day)
          renderer$render(name = "sp_median", value = quantiles[[2]], timestep = day)
          renderer$render(name = "sp_upper", value = quantiles[[3]], timestep = day)
          renderer$render(name = "sp_mean", value = mean(x = sp), timestep = day)
          
        }
      }
    }
    
    # processes
    processes <- list(
      independent_ab_titre_process(parameters=parameters, variables = variables, dt = dt),
      vaccination_process(parameters = parameters,variables = variables,events = events, dt = dt),
      infection_process_vaccine_cpp(parameters = parameters,variables = variables,events = events, dt = dt),
      categorical_count_renderer_process_daily(renderer = renderer, variable = variables$states, categories = variables$states$get_categories(),dt = dt),
      double_count_render_process_daily(renderer = nat_renderer, variable = variables$ab_titre, dt = dt ),
      double_count_render_process_daily(renderer = nat_inf_renderer, variable = variables$ab_titre_inf, dt = dt ),
      sp_render_process_daily(renderer = sp_renderer, variable1 = variables$ab_titre, variable2 = variables$ab_titre_inf, dt = dt),
      integer_count_render_process_daily(renderer = vaxx_renderer, variable = variables$dose_num, margin = 1:vaccine_doses, dt = dt))
    
    setup_events(parameters = parameters, events=events, variables = variables, dt=dt)
    
    simulation_loop_safir(
      variables = variables,
      events = events,
      processes = processes,
      timesteps = timesteps,
      variables_dont_update = c("discrete_age", "phase"),
      progress = TRUE
    )
    
    
    df <- renderer$to_dataframe()
    df_vacc <- vaxx_renderer$to_dataframe()
    df_inc <- incidence_renderer$to_dataframe()
    df_nat <- nat_renderer$to_dataframe() %>%
      rename(vax_ab_mean = mean,
             vax_ab_median = median,
             vax_ab_lower = lower,
             vax_ab_upper = upper)
    df_nat_inf <- nat_inf_renderer$to_dataframe() %>%
      rename(nat_ab_mean = mean,
             nat_ab_median = median,
             nat_ab_lower = lower,
             nat_ab_upper = upper)
    df_sp <- sp_renderer$to_dataframe()
    
    df_inc <- df_inc %>%
      mutate(timestep = floor((timestep-1)*dt)) %>% 
      mutate(incidence = coalesce(incidence,0))%>%
      group_by(timestep) %>%
      summarise(incidence = sum(incidence))
    
    df_hosp <- process_hosp_renderers(renderers = hosp_render, parameters = parameters)
    
    df_hosp <- df_hosp %>%
      mutate(hosp = hosp_get_live + hosp_get_die,
             hosp_all = hosp + hosp_not_get_live + hosp_not_get_die,
             ICU = ICU_get_live + ICU_get_die,
             ICU_all = ICU + ICU_not_get_live + ICU_not_get_die,
             timestep = day-1) %>%
      select(timestep,hosp,hosp_all,ICU,ICU_all)
    
    df <- left_join(df, df_vacc, by = c("timestep"))
    df <- left_join(df, df_inc, by = c("timestep"))
    df <- left_join(df, df_nat, by = c("timestep"))
    df <- left_join(df, df_nat_inf, by = c("timestep"))
    df <- left_join(df, df_sp, by = c("timestep"))
    df <- left_join(df, df_hosp, by = c("timestep"))
    
    # Save output
    output_path <- paste0("raw_outputs/output_", name)
    ifelse(!dir.exists(file.path("raw_outputs", paste0("output_", name))), dir.create(file.path("raw_outputs", paste0("output_", name))), NA)
    saveRDS(df, paste0(output_path, "/scenario_", scenario, ".rds"))
    
  }