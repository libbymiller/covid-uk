library(lubridate)

school_iv_set_contact_matrix = c(1, 1, 0, 1,  1, 1, 0, 1,  1)

run_simulation = function(r, R0, arguments, model_structs, dyn, tots, dump=FALSE)
{  
  parameters = model_structs$parameters
  core_param = model_structs$unmodified

  cat(paste0("[Values]:\n\tR0 : ", R0, "\n"));

  # 1. Pick age-varying symptomatic rate
  covid_scenario = arguments$age_var_symptom_rates
  cat(paste("[Calculating R0 Adjustment]:\n"))
  covy = unname(unlist(covid_scenario[sample.int(nrow(covid_scenario), 1), f_00:f_70]));
  covy = rep(covy, each = 2);
  cat(paste("\tAdjusted y param : ", core_param$pop[[1]]$y, " -> ", covy, "\n"))

  # 2. Calculate R0 adjustment needed
  core_param$pop[[1]]$y = covy;
  u_adj = R0 / cm_calc_R0(core_param, 1);
  cat(paste("\tAdjusted u param : ", u_adj, "\n"))

  # 3. Pick seeding times
  cat(paste("[Selecting Seeding Times]:\n"))
  seed_start = ifelse(uk_pop_struct$london, sample(0:6, length(uk_pop_struct$london), replace = TRUE), 
                              sample(0:20, length(uk_pop_struct$london), replace = TRUE));
  cat(paste("\tDone.\n"))  

  # 4. Do base model
  
  # 4a. Set parameters
  cat(paste("[Duplicating and Setting Parameters]:\n"))
  params = duplicate(parameters);
  for (j in seq_along(params$pop)) {
    params$pop[[j]]$u = params$pop[[j]]$u * u_adj;
    params$pop[[j]]$y = covy;
    params$pop[[j]]$seed_times = rep(seed_start[j] + 0:27, each = 2);
    params$pop[[j]]$dist_seed_ages = cm_age_coefficients(as.numeric(arguments$seed$min_age), as.numeric(arguments$seed$max_age), 5 * 0:16);
  }
  cat(paste("\tDone.\n"))

  if(arguments$run_mode$mode == "R0 Analysis")
  {
    # CALCULATE IMPACT ON R0
    cat(paste("[Calculating impact on R0]:\n"))
    for (i in seq_along(arguments$intervention))
    {
      iR0s = rep(0, length(params$pop));
      iweights = rep(0, length(params$pop));
      for (j in seq_along(params$pop))
      {
        for (k in seq_along(arguments$intervention[[i]]))
        {
          params$pop[[j]][[names(arguments$intervention[[i]])[k]]] = arguments$intervention[[i]][[k]];
        }
        iR0s[j] = cm_calc_R0(params, j);
        iweights[j] = sum(params$pop[[j]]$size);
      }
      
      weighted_R0 = weighted.mean(iR0s, iweights);
      dynamics = rbind(dynamics, data.table(run = r, scenario = names(arguments$intervention)[i], R0 = weighted_R0));
  
    }

    cat(paste("\tDone.\n"))
    cat(paste("[Calculating R0 Weighted Mean and Initial Dynamics]: "))
    weighted_R0 = weighted.mean(iR0s, iweights);
    dyn = rbind(dyn, data.table(run = r, scenario = names(arguments$intervention), R0 = weighted_R0));
    cat(paste("\n\tWeighted Mean : ", weighted_R0, "\n"))

    return(0)
  }

  # 4b. Set school terms
  cat(paste("[Setting School Terms]:\n"))
  iv = cm_iv_build(params)
  cm_iv_set(iv, arguments$school_terms$close, arguments$school_terms$reopen, contact = school_iv_set_contact_matrix, trace_school = 2);
  params = cm_iv_apply(params, iv);
  cat(paste("\tDone.\n"))

  if(dump)
  {
    output_file = file.path(covid_uk_path, "output", paste0("mod-params-", gsub(" ", "", gsub(":","",Sys.time())), ".pars"))
    dput(parameters, file=output_file)
    message(paste0("[Test Mode Abort]:\n\tParams saved to '", output_file,"'.\n"))
    return(0)
  }

  # 4c. Run model
  cat(paste("[Running Non-Lockdown Simulation]:\n"))
  run = cm_simulate(params, 1, r);
  run$dynamics[, run := r];
  run$dynamics[, scenario := "Base"];
  run$dynamics[, R0 := R0s[r]];
  cat("\tCombining Totals: ")
  tots = add_totals(run, tots);
  cat("\tDone\n\tCombining Dynamics: ")
  print(c(length(run), length(dyn), length(iv)))
  dyn = add_dynamics(run, dyn, iv);
  cat("\tDone\n\tDetermining Peak: ")
  peak_t = run$dynamics[compartment == "cases", .(total_cases = sum(value)), by = t][, t[which.max(total_cases)]];
  peak_t_bypop = run$dynamics[compartment == "cases", .(total_cases = sum(value)), by = .(t, population)][, t[which.max(total_cases)], by = population]$V1;
  cat("\tDone.\n")
  rm(run)
  gc()

  cat(paste("[Retrieving Lockdown Measures]:\n"))
  cat(paste("\tIntervention: ", arguments$intervention_name, "\n"))
  intervention = arguments$intervention
  cat(paste("\tDuration: ", as.numeric(arguments$lockdown_trigger$duration), "days\n"))
  duration = as.numeric(arguments$lockdown_trigger$duration)
  cat(paste("\tTrigger: ", arguments$lockdown_trigger$trigger, "\n"))
  trigger = arguments$lockdown_trigger$trigger
  cat(paste("\tShift: ", as.numeric(arguments$lockdown_trigger$intervention_shift), "days\n"))
  intervention_shift = as.numeric(arguments$lockdown_trigger$intervention_shift)
  lockdown = ifelse(as.numeric(arguments$lockdown_trigger$icu_bed_usage) != -1, as.numeric(arguments$lockdown_trigger$icu_bed_usage), NA)
  cat(paste("\tICU Bed Usage Trigger: ", lockdown, "\n"))
            
  # 5a. Make parameters and adjust R0
  params = duplicate(parameters);
  for (j in seq_along(params$pop)) {
    params$pop[[j]]$u = params$pop[[j]]$u * u_adj;
    params$pop[[j]]$y = covy;
    if (!is.na(lockdown)) {
      params$pop[[j]]$observer = observer_lockdown(lockdown);
    }
  }
            
  # 5b. Set interventions
  if (trigger == "national") {
    intervention_start = peak_t - duration / 2 + intervention_shift;
  } else if (trigger == "local") {
    intervention_start = peak_t_bypop - duration / 2 + intervention_shift;
  } else {
    intervention_start = as.numeric(ymd(trigger) - ymd(params$date0));
  }
            
  if (trigger == "local") {
    # Trigger interventions to one population at a time.
    for (pi in seq_along(params$pop)) {
      ymd_start = ymd(params$date0) + intervention_start[pi];
      ymd_end = ymd_start + duration - 1;
      iv = cm_iv_build(params)
      cm_iv_set(iv, arguments$school_terms$close, arguments$school_terms$reopen, contact = school_iv_set_contact_matrix, trace_school = 2);
      cm_iv_set(iv, ymd_start, ymd_end, arguments$intervention);
      cm_iv_set(iv, ymd_start, ymd_end, trace_intervention = 2);
      params = cm_iv_apply(params, iv, pi);
    }
  } else {
  # Trigger interventions all at once.
    ymd_start = ymd(params$date0) + intervention_start;
    ymd_end = ymd_start + duration - 1;
    iv = cm_iv_build(params)
    cm_iv_set(iv, arguments$school_terms$close, arguments$school_terms$reopen, contact = school_iv_set_contact_matrix, trace_school = 2);
    cm_iv_set(iv, ymd_start, ymd_end, arguments$intervention);
    cm_iv_set(iv, ymd_start, ymd_end, trace_intervention = 2);
    params = cm_iv_apply(params, iv);
  }
  
  # 5c. Run model
  run = cm_simulate(params, 1, r);

  tag = ifelse(lockdown >= 0, lockdown, "variable");

  run$dynamics[, run := r];
  run$dynamics[, scenario := paste0("", tag)];
  run$dynamics[, R0 := R0s[r]];
  tots = add_totals(run, tot);
  dyn = add_dynamics(run, dy, iv);

  rm(run)
  gc()

  return(1)
}