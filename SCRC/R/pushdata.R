library(reticulate)
library(SCRCdataAPI)
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

prepare_dynamics <- function(t_dynamics, format=table_spec)
{
  table <- make_table(t_dynamics, format)
  base  <- table %>% .[scenario == "Base"] %>% select(statistic, lower, median, upper)
  lockdown <- table %>% .[scenario == "Lockdown"] %>% select(statistic, lower, median, upper)

  return(
    list(
      base = base,
      lockdown = lockdown
    )
  )
}

prepare_totals <- function(t_total)
{
  cases_deaths <- arrange_by_age_and_categ(t_total)
  cases <- cases_deaths %>% .[compartment %like% "Cases"]
  deaths <- cases_deaths %>% .[compartment %like% "Deaths"]
  cases_lockdown <- cases %>% .[scenario == "Lockdown"] %>% select(age_group, lower, median, upper)
  cases_base <- cases %>% .[scenario == "Base"] %>% select(age_group, lower, median, upper)
  deaths_lockdown <- deaths %>% .[scenario == "Lockdown"] %>% select(age_group, lower, median, upper)
  deaths_base <- deaths %>% .[scenario == "Base"] %>% select(age_group, lower, median, upper)

  return(
    list(
      cases = list(base = cases_base, lockdown = cases_lockdown),
      deaths = list(base = deaths_base, lockdown = deaths_lockdown)
    )
  )
}

push_data <- function(data_path, make_csv=FALSE)
{
    file_names <- list(totals = "cases_deaths.h5", dynamics="dynamics.h5")
    # Reformat data before loading as data tables
    dynamics <- reflow_dynamics(qread(paste0(data_path, "-dynamics.qs")))
    totals <- reflow_totals(qread(paste0(data_path, "-totals.qs")))
    
    total_tables <- prepare_totals(totals)
    dynamics_tables <- prepare_dynamics(dynamics)

    for(category in names(total_tables))
    {
      for(table in names(total_tables[[category]]))
      {
        create_table(filename = file_names$totals,
                     path = "output",
                     component = paste0(category,"_",table),
                     df = total_tables[[category]][[table]])
        if(make_csv)
        {
          write.csv(total_tables[[category]][[table]],
                    file=file.path("output", paste0(category,"_", table, ".csv")),
                    row.names=FALSE)
        }
      }
    }
    for(category in names(dynamics_tables))
    {
      create_table(filename = file_names$dynamics,
                  path = "output",
                  component = paste0(category),
                  df = dynamics_tables[[category]])
      if(make_csv)
      {
        write.csv(dynamics_tables[[category]],
                  file=file.path("output", paste0(category,"_dynamics.csv")),
                  row.names=FALSE)
      }
    }
}