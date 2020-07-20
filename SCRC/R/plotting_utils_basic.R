#
# Basic utility functions for making summary plots
# These functions were extracted from UK-view.R, but have not been modified
# 

# ---- Basic transformations --------------------------------------------------
# Friendly number
friendly = function(x, digits = 2)
{
  ifelse(x == 0, 0,
         ifelse(x > 1000000, paste(signif(x / 1000000, digits), "M"),
                ifelse(x > 1000, paste(signif(x / 1000, digits), "k"),
                       signif(x, digits))))
}

# Friendly y axis labels
axis_friendly = function(breaks)
{
  if (max(breaks, na.rm = T) > 1000000) {
    return (ifelse(breaks == 0, 0, paste(breaks / 1000000, "M")))
  } else if (max(breaks, na.rm = T) > 1000) {
    return (ifelse(breaks == 0, 0, paste(breaks / 1000, "k")))
  }
  return (breaks)
}

# Friendly x axis labels for dates
axis_date = function(breaks)
{
  ifelse(month(breaks) == 1 | breaks == breaks[!is.na(breaks)][1], format(breaks, "%Y %b"), format(breaks, "%b"));
}


# amalgamate user compartments into more friendly ones
reflow_dynamics = function(d)
{
  d[compartment == "icu_p", compartment := "beds_icu"];
  d[compartment == "nonicu_p", compartment := "beds_nonicu"];
  d[compartment == "death_o", compartment := "deaths"];
}

reflow_totals = function(t)
{
  t[compartment == "icu_p", compartment := "beds_icu"];
  t[compartment == "nonicu_p", compartment := "beds_nonicu"];
  t[compartment == "death_o", compartment := "deaths"];
}

# summarise distributions
median_ci = function(x, conf = 0.95)
{
  y = quantile(x, c((1 - conf) / 2, 0.5, 1 - (1 - conf) / 2));
  y = as.list(y);
  names(y) = c("lower", "median", "upper");
  return (y)
}


# ---- Summary tables ---------------------------------------------------------
# save summary tables as csv
save_table = function(tb, filename)
{
  tb2 = dcast(tb, statistic ~ scenario, value.var = "value_str");
  fwrite(tb2, filename)
}

plot_table = function(tb, nrow = NULL)
{
  ggplot(tb) +
    geom_pointrange(aes(x = scenario, y = median, ymin = lower, ymax = upper, colour = scenario), size = 0.25, fatten = 0.2) +
    facet_wrap(~statistic, scales = "free", nrow = nrow) +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = axis_friendly, limits = c(0, NA)) +
    theme(strip.background = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
}


# ---- Colours ---------------------------------------------------------
gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}