add_totals = function(run, totals)
{
  regions = run$dynamics[, unique(population)];
  
  # totals by age
  totals0 = run$dynamics[, .(total = sum(value)), by = .(scenario, run, compartment, group)];
  return (rbind(totals, totals0))
}

add_dynamics = function(run, dynamics, iv)
{
  regions = run$dynamics[, unique(population)];
  
  interv = data.table(scenario = run$dynamics$scenario[1], run = run$dynamics$run[1], t = unique(run$dynamics$t), 
                      compartment = "trace_school", region = "All", value = unlist(iv$trace_school));
  if (!is.null(iv$trace_intervention)) {
    interv = rbind(interv,
                   data.table(scenario = run$dynamics$scenario[1], run = run$dynamics$run[1], t = unique(run$dynamics$t), 
                              compartment = "trace_intervention", region = "All", value = unlist(iv$trace_intervention)));
  } else {
    interv = rbind(interv,
                   data.table(scenario = run$dynamics$scenario[1], run = run$dynamics$run[1], t = unique(run$dynamics$t), 
                              compartment = "trace_intervention", region = "All", value = 1));
  }
  
  csvlines = NULL;
  if (nchar(run$csv[[1]]) > 0) {
    csvlines = fread(run$csv[[1]], header = F);
    csvlines = cbind(run$dynamics$scenario[1], run$dynamics$run[1], csvlines);
    names(csvlines) = c("scenario", "run", "t", "compartment", "region", "value");
    csvlines = unique(csvlines);
  }
  
  # time courses
  return (rbind(dynamics,
                run$dynamics[population %in% locations[westmid],  .(region = "West Midlands",    value = sum(value)), by = .(scenario, run, t, compartment)],
                run$dynamics[population %in% locations[cumbria],  .(region = "Cumbria",          value = sum(value)), by = .(scenario, run, t, compartment)],
                run$dynamics[population %in% locations[london],   .(region = "London",           value = sum(value)), by = .(scenario, run, t, compartment)],
                run$dynamics[population %in% locations[england],  .(region = "England",          value = sum(value)), by = .(scenario, run, t, compartment)],
                run$dynamics[population %in% locations[wales],    .(region = "Wales",            value = sum(value)), by = .(scenario, run, t, compartment)],
                run$dynamics[population %in% locations[scotland], .(region = "Scotland",         value = sum(value)), by = .(scenario, run, t, compartment)],
                run$dynamics[population %in% locations[nireland], .(region = "Northern Ireland", value = sum(value)), by = .(scenario, run, t, compartment)],
                run$dynamics[,                                    .(region = "United Kingdom",   value = sum(value)), by = .(scenario, run, t, compartment)],
                interv,
                csvlines
  ))
}

# Calculate R0 for a given population
cm_calc_R0 = function(time_step, parameters) {
    po = parameters
    dIp = sum(po$dIp * seq(0, by = time_step, length.out = length(po$dIp)));
    dIs = sum(po$dIs * seq(0, by = time_step, length.out = length(po$dIs)));
    dIa = sum(po$dIa * seq(0, by = time_step, length.out = length(po$dIa)));

    cm = Reduce('+', mapply(function(c, m) c * m, po$contact, po$matrices, SIMPLIFY = F));

    ngm = po$u * t(t(cm) * (
        po$y * (po$fIp * dIp + po$fIs * dIs) + 
        (1 - po$y) * po$fIa * dIa)
    )

    abs(eigen(ngm)$values[1])
}

# return translated parameters to work with the backend,
# i.e. fix any times expressed in dates to be expressed in days since date0.
cm_translate_parameters = function(p)
{
  translate_time = function(t) {
      if (is.numeric(t)) {
          return (t)
      } else {
          return (as.numeric(ymd(t) - ymd(p$date0)));
      }
  }
    
  p$time0 = translate_time(p$time0);
  p$time1 = translate_time(p$time1);
  
  p$pop$seed_times = sapply(p$pop$seed_times, translate_time);
      for (si in seq_along(p$pop$schedule)) {
          p$pop$schedule[[si]]$t = translate_time(p$pop$schedule[[si]]$t);
      }
  
  return (p);
}

cm_iv_build = function(parameters)
{
    p = cm_translate_parameters(parameters)
    data.table(date = (ymd(p$date0) + p$time0):(ymd(p$date0) + p$time1));
}

# construct a delay distribution following a gamma distribution with mean mu and shape parameter shape.
cm_delay_gamma = function(mu, shape, t_max, t_step)
{
    scale = mu / shape;
    t_points = seq(0, t_max, by = t_step);
    heights = pgamma(t_points + t_step/2, shape, scale = scale) - 
        pgamma(pmax(0, t_points - t_step/2), shape, scale = scale);
    return (data.table(t = t_points, p = heights / sum(heights)))
}

# construct a delay distribution that effectively skips the compartment
cm_delay_skip = function(t_max, t_step)
{
    t_points = seq(0, t_max, by = t_step);
    return (data.table(t = t_points, p = c(1, rep(0, t_max / t_step))))
}


# for a set of age group bounds age_bounds (including the lower and upper bound as well as intermediate bounds),
# return a vector giving length(age_bounds) - 1 numerical entries between 0 and 1 inclusive, depending on how much
# each age group lies within the range age_min to age_max.
cm_age_coefficients = function(age_min, age_max, age_bounds)
{
    x = rep(0, length(age_bounds) - 1);
    for (ag in 1:(length(age_bounds) - 1)) {
        ag_lower = age_bounds[ag];
        ag_upper = age_bounds[ag + 1];
        ag_0 = max(age_min, ag_lower);
        ag_1 = min(age_max, ag_upper);
        x[ag] = max(0, (ag_1 - ag_0) / (ag_upper - ag_lower));
    }
    return (x);
}