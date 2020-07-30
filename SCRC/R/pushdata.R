library(reticulate)
library(SCRCdataAPI)
library(progress)
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

prepare_dynamics_summary <- function(t_dynamics, format=table_spec)
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

prepare_dynamics <- function(t_dynamics)
{
  output = list(Base=list(), Lockdown=list())
  time_labels <- NA
  dt <- NA
  for(s in names(output))
  {
    for(c in unique(t_dynamics[,compartment]))
    {
      
      for(r in unique(t_dynamics[,run]))
      {
        time_series <- t_dynamics[scenario == s][run == r][compartment == c] %>%
                       select(., t, value) %>% data.table::transpose(.)

        if(is.na(time_labels))
        {
          time_labels <- time_series[1,]
          dt <- data.table(matrix(nrow = 0, ncol = length(time_labels)))
        }

        time_series <- time_series[2,]

        dt <- rbind(time_series, dt, use.names=FALSE)
        
      }
      output[[s]][[c]] = dt
    }   
  }

  return(output)
}

prepare_totals <- function(t_total)
{
  cases_deaths <- arrange_by_age_and_categ(t_total)

  cases <- cases_deaths %>% .[compartment %like% "Cases"]

  deaths <- cases_deaths %>% .[compartment %like% "Deaths"]

  cases_lockdown <- cases %>% 
                    .[scenario == "Lockdown"] %>%
                    select(age_group, lower, median, upper)

  cases_base <- cases %>%
                .[scenario == "Base"] %>%
                select(age_group, lower, median, upper)

  deaths_lockdown <- deaths %>%
                     .[scenario == "Lockdown"] %>%
                     select(age_group, lower, median, upper)

  deaths_base <- deaths %>%
                 .[scenario == "Base"] %>%
                 select(age_group, lower, median, upper)

  return(
    list(
      cases = list(base = cases_base, lockdown = cases_lockdown),
      deaths = list(base = deaths_base, lockdown = deaths_lockdown)
    )
  )
}

push_data <- function(data_path, make_csv=FALSE)
{
    out_str = c("[Formatting Data]:\n", "\tRetrieving data\n")
    cat(out_str)
    file_names <- list(totals = "cases_deaths.h5",
                       dynamics="dynamics_summary.h5",
                       time_series="dynamics_timeseries.h5")

    # Reformat data before loading as data tables
    dynamics <- reflow_dynamics(qread(paste0(data_path, "-dynamics.qs")))
    totals <- reflow_totals(qread(paste0(data_path, "-totals.qs")))

    cat("\tFormatting Totals\n")
    total_tables <- prepare_totals(totals)
    cat("\tBuilding Dynamics time series\n")    
    dynamics_time_series <- prepare_dynamics(dynamics)
    cat("\tSummarising Dynamics\n")
    dynamics_tables <- prepare_dynamics_summary(dynamics)

    cat("[Saving to HDF5]:\n")
    cat("\tSaving Totals\n")
    prog_totals = progress_bar$new(total=100)
    for(category in names(total_tables))
    {
      for(table in names(total_tables[[category]]))
      {
        prog_totals$tick()
        create_table(filename = file_names$totals,
                     path = "output",
                     component = paste0(category,"_",table),
                     df = total_tables[[category]][[table]]
                     )
        if(make_csv)
        {
          write.csv(total_tables[[category]][[table]],
                    file=file.path("output", paste0(category,"_", table, ".csv")),
                    row.names=FALSE)
        }
      }
    }
    cat("\n\tSaving Dynamics Summary\n")
    prog_dyn_sum = progress_bar$new(total=100)
    for(category in names(dynamics_tables))
    {
      prog_dyn_sum$tick()
      create_table(filename = file_names$dynamics,
                  path = "output",
                  component = paste0(category),
                  df = dynamics_tables[[category]]
                  )
      if(make_csv)
      {
        write.csv(dynamics_tables[[category]],
                  file=file.path("output", paste0(category,"_dynamics.csv")),
                  row.names=FALSE)
      }
    }
    cat("\n\tSaving Dynamics Time Series\n")
    prog_dyn_ts = progress_bar$new(total=100)
    for(scenario in names(dynamics_time_series))
    {
      for(compartment in names(dynamics_time_series[[scenario]]))
      {
        prog_dyn_ts$tick()
        create_table(filename = file_names$time_series,
                     path = "output",
                     component = paste(scenario, compartment, sep="_"),
                     df = dynamics_time_series[[scenario]][[compartment]]
                     )
        if(make_csv)
        {
          write.csv(dynamics_time_series[[scenario]][[compartment]],
                    file=file.path("output", paste(scenario, compartment, "time", "series.csv", sep="_")),
                    row.names=FALSE)
        }
      }
      cat("\n")
    }
}