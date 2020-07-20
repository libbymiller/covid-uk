# - - - - - - - - - - - - - - - - - - - - - - - 
# UK model: plot outputs
# - - - - - - - - - - - - - - - - - - - - - - -

suppressPackageStartupMessages({
  library(cowplot)
  library(stringr)
  library(rlang)
  library(data.table)
})

# Load requested settings from command line
# - Takes a single argument required argument: the base name of the output to plot
#     - for example, specifying "run-Other" will plot data contained in 
#       output/run-Other-1-dynamics.qs and output/run-Other-1-totals.qs
# - A further optional argument "--covid-uk-path=xyz" can be used to specify 
#   the base directory within which to operate
argv = commandArgs(trailingOnly = TRUE)

run_id <- argv[1]


# Set paths
# Set this path to the base directory of the repository.
covid_uk_search = grep('--covid-uk-path*', argv, value = TRUE)

if(length(covid_uk_search) > 0) {
  covid_uk_path = strsplit(covid_uk_search, split = '=')[[1]][[2]];
} else {
  covid_uk_path = getwd();
}

cm_path = file.path(covid_uk_path, "covidm")
scrc = file.path(covid_uk_path, "SCRC")

# Import "vanilla" covidm libraries
source(file.path(cm_path, "R", "covidm.R"))

# Import utility and plotting functions 
source(file.path(scrc, "R", "plotting_utils_basic.R"))
source(file.path(scrc, "R", "plotting_utils_adapted.R"))


# set theme
theme_set(theme_cowplot(font_size = 7, line_size = 0.25))


# load data
data_path <- file.path(covid_uk_path, "output", run_id)

dynamics <- reflow_dynamics(qread(paste0(data_path, "-dynamics.qs")))
totals <- reflow_totals(qread(paste0(data_path, "-totals.qs")))


# Make plots
# - summary stats (see docs of make_table() for an explanation of table_spec and how to change the statistics calculated)
table_spec = fread(
  "compartment, stat, time
  cases, total, t
  deaths, total, t
  cases, peak, week
  deaths, peak, week
  beds_icu, peak, t
  beds_nonicu, peak, t
  cases, peak_time, week
  trace_lockdown, lockdown_duration, t
  trace_intervention, lockdown_duration, t
  trace_schools, lockdown_duration, t
  S, total_end, t")

tb1 = make_table(dynamics, table_spec)
pl1 = plot_table(tb1)


# - cases & deaths by age group
pl2 = plot_attackrate(totals)


# - epidemiological curves (compartment sizes over time)
pl3 = plot_epi(dynamics, totals, (0:10)/10, "2020-01-29", "2020-10-15")


# Combine plots and save
f = plot_grid(pl1, pl2, pl3, ncol = 1, rel_heights = c(6, 6, 10), labels = c("a", "b", "c"), label_size = 9);

ggsave(paste0(data_path, "_plots.pdf"), f, width = 20, height = 22, units = "cm", useDingbats = F);
save_table(tb1, paste0(data_path, "_summary_stats.csv"));
