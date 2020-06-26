# Observer for lockdown scenarios
observer_lockdown = function(arguments, lockdown_trigger) function(time, dynamics)
{
  # Get current icu prevalence
  icu_prevalence = dynamics[t == time, sum(icu_p)];
  
  # Determine lockdown trigger
  trigger = lockdown_trigger;
  
  # If ICU prevalence exceeds a threshold, turn on lockdown

  ldown_r = arguments$lockdown_rates

  if (icu_prevalence >= trigger) {
    return (list(csv = paste(time, "trace_lockdown", "All", 2, sep = ","),
                 changes = list(contact_lowerto = c(ldown_r$home, ldown_r$work, ldown_r$schools, ldown_r$other, 
                                                    ldown_r$home_elderly, ldown_r$work_elderly, ldown_r$schools_elderly,
                                                    ldown_r$other_elderly,  ldown_r$child_elderly))));
  } else  {
    return (list(csv = paste(time, "trace_lockdown", "All", 1, sep = ","),
                 changes = list(contact_lowerto = c(1, 1, 1, 1, 1, 1, 1, 1, 1))));
  }
  return (list(csv = paste(time, "trace_lockdown", "All", 1, sep = ",")))
}