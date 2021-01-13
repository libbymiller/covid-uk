##############################################################################
#                                                                            #
#                         API Model Output Handling                          #
#                                                                            #
#   Assembly and storage of results using the SCRC data pipeline.            #
#                                                                            #
#   @author : K. Zarebski (UKAEA)                                            #
#   @date : last modified 2020-08-03                                         #
#                                                                            #
##############################################################################

library(reticulate)  # Run Python commands within R
library(magrittr)    # For special two way pipes
library(progress)    # For progress bar

# Determine Python binary from the current 'which python3' command result
python_version <- system("which python3", intern=TRUE)
use_python(python_version)

# DataFrame class for conversion to Python-friendly type
pandas_df <- import("pandas")$DataFrame

# Import utility and plotting functions 
scrc = file.path(covid_uk_path, "SCRC")
source(try_loc(file.path(scrc, "R", "plotting_utils_adapted.R")))
source(try_loc(file.path(scrc, "R", "plotting_utils_basic.R")))

# Table specification for formatting outputs
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

#' Reformat/Prepare Dynamics Summary
#' 
#' Reformats output to give summary of the dynamics from a model run
#' 
#' @param t_dynamics Dynamics table from output
#' @param format Format of output table
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

#' Prepare/Format Dynamics Time Series Data
#' 
#' Creates a table showing the dynamics data over time
#' for time series based outputs
#' 
#' @param t_dynamics Dynamics table from output
prepare_dynamics <- function(t_dynamics)
{
  # Iterate through Base, Lockdown scenarios
  # and fetch the time series data for each compartment
  output = list(Base=list(), Lockdown=list())
  for(s in names(output))
  {
    for(c in unique(t_dynamics[,compartment]))
    {
      dt <- NA
      time_labels <- NA
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

#' Prepare/Format totals data
#' 
#' Reformat the totals output from the model run into
#' a summary table
#' 
#' @param t_total Totals table from output
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

#' Send the Data to the API
#' 
#' Uses the API to create HDF5 files to store the output
#' for both dynamics and totals
#' 
#' @param data_path Location of output qs files
#' @param config_path Location of config.yaml API file
#' @param standard_api Instance of standard API object
#' @param make_csv Option to create CSV files also (Default: FALSE)
push_data <- function(data_path, config_path, standard_api, make_csv=FALSE)
{
    output_location <- config_path %>%
                       gsub("/config.yaml", "", .)

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
    cat(c("\tSaving Totals to Folder : ",
              file.path(output_location,"lshtm_outputs", "cases_deaths"), "\n"))
    prog_totals = progress_bar$new(total=length(total_tables)*length(total_tables[[1]]))
    for(category in names(total_tables))
    {
      for(table in names(total_tables[[category]]))
      {
        prog_totals$tick()

        # Need to change first column from factor to character vector
        total_tables[[category]][[table]]$age_group %<>% sapply(., as.character)
        pd_table <- pandas_df(total_tables[[category]][[table]])

        standard_api$write_table("lshtm_outputs/cases_deaths", paste0(category,"_",table), pd_table)

        if(make_csv)
        {
          write.csv(total_tables[[category]][[table]],
                    file=file.path("output", paste0(category,"_", table, "_", "totals.csv")),
                    row.names=FALSE)
        }
      }
    }

    cat(paste("\n\tSaving Dynamics Summary to Folder : ", 
              file.path(output_location,"lshtm_outputs", "dynamics_summary"), "\n"))
    prog_dyn_sum = progress_bar$new(total=length(dynamics_tables))
    for(category in names(dynamics_tables))
    {
      prog_dyn_sum$tick()
      dynamics_tables[[category]]$statistic %<>% sapply(., as.character)
      pd_table <- pandas_df(dynamics_tables[[category]])
      standard_api$write_table("lshtm_outputs/dynamics_summary", 
                                           paste0(category), pd_table)

      if(make_csv)
      {
        write.csv(dynamics_tables[[category]],
                  file=file.path("output", paste0(category,"_dynamics_summary.csv")),
                  row.names=FALSE)
      }
    }
    cat(paste("\n\tSaving Dynamics Time Series to Folder : ", 
              file.path(output_location, "lshtm_outputs", "dynamics_time_series"), "\n"))
    prog_dyn_ts = progress_bar$new(total=length(dynamics_time_series)*length(dynamics_time_series[[1]]))
    for(scenario in names(dynamics_time_series))
    {
      for(compartment in names(dynamics_time_series[[scenario]]))
      {
        prog_dyn_ts$tick()
        colnames(dynamics_time_series[[scenario]][[compartment]]) <- as.character(1:ncol(dynamics_time_series[[scenario]][[compartment]]))

        pd_table <- pandas_df(dynamics_time_series[[scenario]][[compartment]])
        standard_api$write_table("lshtm_outputs/dynamics_time_series", 
                                           paste(scenario, compartment, 
                                                 "time", "series", sep="_"), 
                                              pd_table)
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
