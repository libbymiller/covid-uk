library(reticulate)
use_python("/home/kristian/venvs/lshtm/bin/python")

api_py <- import("data_pipeline_api.standard_api")$StandardAPI
StandardAPI <- function(config_loc)
{
    return(api_py(config_loc, "test_uri", "test_git_sha"))
}

# Import utility and plotting functions 
scrc = file.path(covid_uk_path, "SCRC")
source(file.path(scrc, "R", "plotting_utils_adapted.R"))
source(file.path(scrc, "R", "plotting_utils_basic.R"))

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
  S, total_end, t"
)

push_data <- function(data_path)
{
    # Reformat data before loading as data tables
    dynamics <- reflow_dynamics(qread(paste0(data_path, "-dynamics.qs")))
    totals <- reflow_totals(qread(paste0(data_path, "-totals.qs")))
    tb1 = make_table(dynamics, table_spec)
    
}