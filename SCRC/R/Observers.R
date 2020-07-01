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
    return (list(csv = paste(time, "trace_lockdown", 2, sep = ","),
                 changes = list(contact_lowerto = ldown_r)));
  } else  {
    return (list(csv = paste(time, "trace_lockdown", 1, sep = ","),
                 changes = list(contact_lowerto = c(1, 1, 1, 1, 1, 1, 1, 1, 1))));
  }
  return (list(csv = paste(time, "trace_lockdown", 1, sep = ",")))
}