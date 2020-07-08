#
# Adapted plotting functions for making summary plots
# Extracted from UK-view.R and adapted to the SCRC pipeline
# 


#' Summarise epidemic dynamics
#' 
#' @param d `data.table` of epidemic dynamics over time
#' @param table_spec `data.table` specifying how each compartment in `d` should be summarised (see details)
#' @return summary `data.table` matching `table_spec`
#' 
#' @details
#' `table_spec` should contain the following columns:
#' - `compartment` - lists compartments in `d` to be summarised
#' - `stat` - the summary statistic required for this compartment. Valid options are "total", "peak", 
#'            "peak_time", "lockdown_duration" and "total_end"
#' - `time` - the time resolution at which the summary should be calculated. Valid options are "t" (days)
#'            or "week" (7 day increments, or weekly if t = 1 is assumed to be the start of a week)
#' 
make_table = function(d, table_spec)
{
  d[, week := t %/% 7]
  results = NULL
  
  for (spec in seq_len(nrow(table_spec)))
  {
    comp = table_spec[spec, compartment];
    stat = table_spec[spec, stat];
    time = table_spec[spec, time];
    
    if (stat == "total") {
      res = d[compartment == comp, .(x = sum(value)), by = .(scenario, run, region)];
      res[, statistic := paste(stat, comp)];
      res = res[, median_ci(x), by = .(scenario, region, statistic)];
      stat_nice = paste("Total", comp);
      
    } else if (stat == "peak") {
      res = d[compartment == comp, .(x = sum(value)), by = c("scenario", "run", time, "region")];
      res = res[, .(x = max(x)), by = .(scenario, run, region)];
      res[, statistic := paste(stat, comp)];
      res = res[, median_ci(x), by = .(scenario, region, statistic)];
      stat_nice = ifelse(time == "t", paste("Peak", comp, "required"),
                         paste(comp, "in peak week"));
      
    } else if (stat == "peak_time") {
      res = d[compartment == comp, .(x = sum(value)), by = c("scenario", "run", time, "region")];
      res = res[, .(x = get(time)[which.max(x)]), by = .(scenario, run, region)];
      res[, statistic := paste(stat, comp)];
      res = res[, median_ci(x), by = .(scenario, region, statistic)];
      stat_nice = paste("Time to peak", comp, ifelse(time == "t", "(days)", "(weeks)"));
      
    } else if (stat == "lockdown_duration") {
      if (d[compartment == comp, .N] == 0) { 
        warning("requested compartment '", comp, "' not present - skipping computation")
        next;
      }
      res = d[compartment == comp, .(x = mean(value - 1)), by = .(scenario, run, region)];
      res[, statistic := paste(stat, comp)];
      res = res[, median_ci(x), by = .(scenario, region, statistic)];
      stat_nice = paste("Proportion of time spent in", comp);
      
    } else if (stat == "total_end") {
      res = d[compartment == comp & t == max(t), .(x = sum(value)), by = .(scenario, run, region)];
      res[, statistic := paste(stat, comp)];
      res = res[, median_ci(x), by = .(scenario, region, statistic)];
      stat_nice = paste("Number of", comp, "at simulation end");
      
    } else {
      stop("Unrecognised stat.");
    }
    
    stat_nice = str_to_sentence(stat_nice);
    stat_nice = str_replace(stat_nice, "beds_icu", "ICU beds");
    stat_nice = str_replace(stat_nice, "beds_nonicu", "non-ICU beds");
    stat_nice = str_replace(stat_nice, "trace_lockdown", "lockdown");
    stat_nice = str_replace(stat_nice, "trace_intervention", "intervention");
    stat_nice = str_replace(stat_nice, " s ", " susceptibles ");
    res[, statistic := stat_nice]
    results = rbind(results, res);
  }
  
  results[, value_str := 
            paste0(friendly(median), " (", friendly(lower), "–", friendly(upper), ")")];
  results[, statistic := factor(statistic, levels = unique(statistic))]
  results[, scenario := factor(scenario, levels = unique(scenario))]
  results
}


#' Plot infections and deaths by age group
#' @param t `data.table` of totals by age group
#' 
#' @return a `ggplot`
#' 
plot_attackrate = function(t)
{
  ts = t;
  ts[, age_lower := (as.numeric(str_replace(group, "([0-9]+)(-|\\+).*", "\\1")) %/% 10) * 10];
  ts[, age_group := paste0(age_lower, "-", age_lower + 9)];
  ts[age_lower == 70, age_group := "70+"];
  ts[, age_group := factor(age_group, levels = unique(age_group))];
  
  ts[, total := sum(total), by = .(scenario, run, compartment, age_group)];
  ts = ts[compartment %in% c("cases", "deaths"), median_ci(total), by = .(scenario, compartment, age_group)];
  ts[, compartment := paste(str_to_sentence(as.character(compartment)), "(thousands)")];
  ts[, scenario := factor(scenario, levels = unique(scenario))];
  
  ggplot(ts) +
    geom_col(aes(x = age_group, y = median / 1000, fill = scenario)) +
    geom_linerange(aes(x = age_group, ymin = lower / 1000, ymax = upper / 1000), size = 0.25) +
    facet_grid(compartment~scenario, switch = "y", scales = "free") +
    labs(x = NULL, y = NULL) +
    theme(strip.background = element_blank(), strip.placement = "outside",
          axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}


#' Plot compartment sizes over time
#' @param d0 `data.table` of epidemic dynamics over time
#' @param t `data.table` of totals
#' @param quant vector of quantiles for which traces should be plotted - the median (0.5) is always plotted
#' @param ymd_start the date corresponding to `t = 1`, as a string in a format recognized by [lubridate::ymd] (e.g. "yyyy-mm-dd")
#' @param ymd_truncate date at which to truncate the plot
#' @param colours vector of colours passed to [ggplot2::scale_colour_manual]
#' @param maxy y-axis limit - this will set the y-axis limit across all plots (use `NA` to allow y-axes to vary across panels)
#' @param exclude scenarios to exclude from plotting
#' 
#' @return a `ggplot`
#' 
plot_epi = function(d0, t, quant, ymd_start, ymd_truncate = "2050-01-01", colours = NULL, maxy = NA, exclude = NULL)
{
  # Copy data and process
  d = duplicate(d0)
  d[, scenario := factor(scenario, levels = unique(scenario))];
  d[, compartment := factor(compartment, levels = unique(compartment))];
  
  # Choose runs
  quant = union(quant, 0.5);
  mrun = t[scenario == "Base", sum(total), by = run][, which(rank(V1) == round(1 + (.N - 1) * 0.5))];
  qrun = t[scenario == "Base", sum(total), by = run][, which(rank(V1) %in% round(1 + (.N - 1) * quant))];
  d = d[run %in% qrun];
  
  # Merge intervention traces
  trace_school = d[compartment == "trace_school", .(run, t, trace_school = value - 1, scenario)]
  d = merge(d, trace_school, by = c("run", "t", "scenario"), all.x = T);
  d[, trace_school := trace_school * min(maxy, max(value), na.rm = T), by = .(compartment)];
  
  trace_intervention = d[compartment == "trace_intervention", .(run, t, trace_intervention = value - 1, scenario)]
  d = merge(d, trace_intervention, by = c("run", "t", "scenario"), all.x = T);
  d[, trace_intervention := trace_intervention *min(maxy, max(value), na.rm = T) * 0.75, by = .(compartment)];
  
  if (d[compartment == "trace_lockdown", .N > 0]) {
    trace_lockdown = d[run == mrun & compartment == "trace_lockdown", .(t, trace_lockdown = value - 1, scenario)]
    d = merge(d, trace_lockdown, by = c("t", "scenario"), all.x = T);
    d[, trace_lockdown := trace_lockdown * min(maxy, max(value), na.rm = T) * 0.5, by = .(compartment)];
  } else {
    d[, trace_lockdown := 0];
  }
  
  d = d[compartment %in% c("cases", "deaths", "beds_icu", "beds_nonicu")];
  
  # Give nice names
  d[compartment == "cases", compartment := "New cases"];
  d[compartment == "deaths", compartment := "Deaths"];
  d[compartment == "beds_icu", compartment := "ICU beds\nrequired"];
  d[compartment == "beds_nonicu", compartment := "Non-ICU beds\nrequired"];
  d[, compartment := factor(compartment, levels = unique(compartment))];
  
  d[, date := ymd(ymd_start) + t];
  
  # Plot
  plot = ggplot(d[date <= ymd(ymd_truncate) & !(scenario %in% exclude)]) +
    geom_line(aes(x = date, y = value, colour = scenario, group = run), size = 0.25, alpha = 0.35) +
    geom_ribbon(aes(x = date, ymin = 0, ymax = trace_school, group = run, fill = "School holidays"), alpha = 0.5/length(quant)) +
    geom_ribbon(aes(x = date, ymin = 0, ymax = trace_intervention, group = run, fill = "Intervention"), alpha = 0.5/length(quant)) +
    geom_ribbon(aes(x = date, ymin = 0, ymax = trace_lockdown, group = run, fill = "Lockdown"), alpha = 0.5/length(quant)) +
    geom_line(data = d[date <= ymd(ymd_truncate) & run == mrun & !(scenario %in% exclude)], 
              aes(x = date, y = value, colour = scenario), size = 0.6) +
    facet_grid(compartment ~ scenario, switch = "y", scales = "free") +
    scale_y_continuous(labels = axis_friendly, limits = c(0, NA)) +
    scale_x_date(date_breaks = "1 month", labels = axis_date) +
    scale_fill_manual(name = NULL, 
                      values = c("School holidays" = "#0000ff", "Intervention" = "#ff0000", "Lockdown" = "#000000")) +
    guides(colour = FALSE) +
    theme(strip.background = element_blank(), strip.placement = "outside", 
          axis.text.x = element_text(size = 5, angle = 45, hjust = 1)) +
    labs(x = NULL, y = NULL);
  
  if (!is.null(colours)) {
    plot = plot + scale_colour_manual(values = colours)
  }
  if (!is.na(maxy)) {
    plot = plot + coord_cartesian(ylim = c(0, maxy));
  }
  return (plot)
}
