suppressPackageStartupMessages({
  library(lubridate)
  library(testit)
})

run_simulation = function(r, R0, arguments, model_structs, dynamics, totals, dump=FALSE)
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
  cat(paste("\tu correction factor : ", u_adj, "\n"))

  # 3. Pick seeding time
  cat("[Determining Start Day for Seeding Times]: ")
  seed_start = sample(arguments$seed$seeding_start_range, length(parameters$pop), replace=TRUE)
  cat(paste("\n\tSeeding on: Day", seed_start, "\n"))
  


  # 4. Do base model
  
  # 4a. Set parameters
  cat(paste("[Duplicating and Setting Parameters]:\n"))
  params = duplicate(parameters);
  for (j in seq_along(params$pop)) {
    params$pop[[j]]$u = params$pop[[j]]$u * u_adj;
    params$pop[[j]]$y = covy;
    params$pop[[j]]$seed_times = rep(seed_start[[j]]+ 0:27, each = 2);
    params$pop[[j]]$dist_seed_ages = cm_age_coefficients(as.numeric(arguments$seed$min_age), as.numeric(arguments$seed$max_age), 5 * 0:arguments$ngroups);
  }
  cat(paste("\tDone.\n"))

  if(arguments$run_mode == "R0 Analysis")
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
      cat(paste("\tDone.\n"))
      cat(paste("[Calculating R0 Weighted Mean and Initial Dynamics]: "))
      weighted_R0 = weighted.mean(iR0s, iweights);
      dynamics = rbind(dynamics, data.table(run = r, scenario = names(arguments$intervention)[i], R0 = weighted_R0));
      cat(paste("\n\tWeighted Mean : ", weighted_R0, "\n"))
    }

    return(0)
  }

  # 4b. Set school terms
  cat(paste("[Setting School Terms]:\n"))
  iv = cm_iv_build(params)
  cm_iv_set(iv, arguments$school_terms$close, arguments$school_terms$reopen, contact = arguments$school_holiday_rates, trace_school = 2);
  params = cm_iv_apply(params, iv);
  cat(paste("\tDone.\n"))

  if(dump)
  {
    output_file = file.path(covid_uk_path, "output", paste0("Sample-stage2-params-", gsub(" ", "", gsub(":","",Sys.time())), ".pars"))
    dput(params, file=output_file)
    message(paste0("[Test Mode Abort]:\n\tParams saved to '", output_file,"'.\n"))
    return(list(run_code=0))
  }

  # 4c. Run model
  cat(paste("[Running Non-Lockdown Simulation]:\n"))
  run = cm_simulate(params, 1, r);
  run$dynamics[, run := r];
  run$dynamics[, scenario := "Base"];
  run$dynamics[, R0 := R0s[r]];
  cat("\tCombining Totals: ")
  totals = add_totals(run, totals);
  cat("\tDone\n\tCombining Dynamics: ")
  dynamics = add_dynamics(run, dynamics, iv);
  assert("ERROR: Post-Simulation Dynamics Table Empty", length(dynamics) > 0)
  assert("ERROR: Post-Simulation Totals Table Empty", length(totals) > 0) 
  cat("\tDone\n\tDetermining Peak: ")
  peak_t = run$dynamics[compartment == "cases", .(total_cases = sum(value)), by = t][, t[which.max(total_cases)]];
  peak_t_bypop = run$dynamics[compartment == "cases", .(total_cases = sum(value)), by = .(t, population)][, t[which.max(total_cases)], by = population]$V1;
  cat("\tDone.\n")
  rm(run)
  gc()

  cat(paste("[Retrieving Lockdown Measures]:\n"))
  cat(paste("\tReading Interventions: "))
  intervention = arguments$intervention
  cat(paste("Done.\n\tDuration: ", as.numeric(arguments$lockdown_trigger$duration), "days\n"))
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
      params$pop[[j]]$observer = observer_lockdown(arguments, c(lockdown));
    }
  }
            
  # 5b. Set interventions
  if (trigger == "national") {
    intervention_start = peak_t - duration / 2 + intervention_shift;
    cat(paste("\tIntervention Commences after: ", intervention_start, "days\n"))
  } else if (trigger == "local") {
    intervention_start = peak_t_bypop - duration / 2 + intervention_shift;
    cat(paste("\tIntervention Commences after: ", intervention_start, "days\n"))
  } else {
    cat(paste("\tIntervention Commences after: ", intervention_start, "days\n"))
    intervention_start = as.numeric(ymd(trigger) - ymd(params$date0));
  }
  cat("[Triggering Interventions]: ")       
  if (trigger == "local") {
    # Trigger interventions to one population at a time.
    cat("\n\tTrigger Type : local")
    for (pi in seq_along(params$pop)) {
      ymd_start = ymd(params$date0) + intervention_start[pi];
      ymd_end = ymd_start + duration - 1;
      cat("\n\tStart (YMD): ", ymd_start, "\n")
      cat("\tEnd (YMD): ", ymd_end, "\n")
      iv = cm_iv_build(params)
      cm_iv_set(iv, arguments$school_terms$close, arguments$school_terms$reopen, contact = arguments$school_holiday_rates, trace_school = 2);
      cm_iv_set(iv, ymd_start, ymd_end, arguments$intervention);
      cm_iv_set(iv, ymd_start, ymd_end, trace_intervention = 2);
      params = cm_iv_apply(params, iv, pi);
    }
  } else {
  # Trigger interventions all at once.
    cat("\n\tTrigger Type : national")
    ymd_start = ymd(params$date0) + intervention_start;
    ymd_end = ymd_start + duration - 1;
    cat("\n\tStart (YMD): ", ymd_start, "\n")
    cat("\tEnd (YMD): ", ymd_end, "\n")
    iv = cm_iv_build(params)
    cm_iv_set(iv, arguments$school_terms$close, arguments$school_terms$reopen, contact = arguments$school_holiday_rates, trace_school = 2);
    cm_iv_set(iv, ymd_start, ymd_end, arguments$intervention);
    cm_iv_set(iv, ymd_start, ymd_end, trace_intervention = 2);
    params = cm_iv_apply(params, iv);
  }
  
  # 5c. Run model
  cat(paste("[Running Lockdown Simulation]: \n"))
  run = cm_simulate(params, 1, r);

  run$dynamics[, run := r];
  run$dynamics[, scenario := "Lockdown"];
  run$dynamics[, R0 := R0s[r]];
  totals = add_totals(run, totals);
  dynamics = add_dynamics(run, dynamics, iv);
  assert("ERROR: Post-Simulation Dynamics Table Empty", length(dynamics) > 0)
  assert("ERROR: Post-Simulation Totals Table Empty", length(totals) > 0) 
  rm(run)
  gc()

  return(list(run_code=1, dynamics=dynamics, totals=totals))
}