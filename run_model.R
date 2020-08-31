# - - - - - - - - - - - - - - - - - - - - - - - 
# UK model: load data and analyse scenarios
# - - - - - - - - - - - - - - - - - - - - - - - 

# Required libraries
suppressPackageStartupMessages({
    library(data.table)   # for data.table, an enhanced (and faster) data.frame
    library(ggplot2)      # for plotting
    library(Rcpp)         # for running the C++ model backend
    library(qs)           # for qsave and qread, faster equivalents of saveRDS and readRDS
    library(lubridate)    # for manipulating dates and times. NB requires stringr
    library(nloptr)       # for numerical optimization
    library(HDInterval)   # for summarizing results
    library(qs)
    library(readxl)
    library(ini)
    library(rlang)
    library(stringr)
    library(tidyverse)
})

# Load requested settings from command line
argv = commandArgs(trailingOnly = TRUE);
argc = length(argv);

out_file = file("current_params.txt")

# List setup of files in one print statement

local_run = grep('--local', argv, value = FALSE)
dump_params = grep('--dump', argv, value = FALSE) # Dump parameters prior to run and exit (for testing)
rebuild = grep('--rebuild', argv, value = FALSE)
help_req = grep('--help', argv, value = FALSE)

help = length(help_req) > 0
local = length(local_run) > 0
dump_params = length(dump_params) > 0
rebuild = length(rebuild) > 0

if(help)
{
  help_str="
USAGE: 

  Rscript run_model.R  <n-runs> [--dump] [--local] [--rebuild] [--help]
                       [--covid-uk-path <path-to-repo>]
                       [--config <path-to-config-file>]

Where: 

  n-runs <int>
    Number of stochastic realisations (model runs) to perform
  
  --dump
    Run model in dump mode, creating parameters then exitting (for debugging purposes)

  --local
    Run model in 'vanilla' mode using the local datasets in a manner closer to the original
    execution method

  --rebuild
    Rebuild the Rcpp source code before running

  --help
    Print this help string

  --covid-uk-path <string>
    Location of the covid-uk repository (if running from an external location)

  --config <string>
    Path to the 'config.yaml' file for data retrieval and creation via the SCRC API 
"
  cat(help_str)
  quit(status=0)
}

n_runs = as.numeric(argv[1]);

if(is.na(n_runs))
{
  stop("Invalid argument for number of runs")
}
covid_uk_search = grep('--covid-uk-path*', argv, value = TRUE)
if(length(covid_uk_search) > 0)
{
  covid_uk_path = strsplit(covid_uk_search, split = '=')[[1]][[2]];
} else {
  covid_uk_path = getwd();
}
config_search = grep('--config*', argv, value = TRUE)
if(length(config_search) > 0)
{
  config_path = strsplit(config_search, split = '=')[[1]][[2]];
} else {
  config_path = config_loc = file.path(covid_uk_path, "SCRC", "pipeline_data", "config.yaml")
}
# covidm options
cm_path = file.path(covid_uk_path, "covidm");
scrc = file.path(covid_uk_path, "SCRC")


# Import "vanilla" covidm libraries
source(file.path(cm_path, "R", "covidm.R"))

# Import SCRC wrapper scripts
source(file.path(scrc, "R", "Utilities.R"))
source(try_loc(file.path(scrc, "R", "Observers.R")))
source(try_loc(file.path(scrc, "R", "BuildStructures.R")))
source(try_loc(file.path(scrc, "R", "Simulate.R")))

if(rebuild)
{
  message("Rebuilding C++ Libraries")
  rebuild_cpp_libraries(cm_path)
}
options_print_str = "[Configuration]:\n"
if(local)
{
  options_print_str = c(options_print_str, "\tSource : Local\n")
  source(try_loc(file.path(scrc, "R", "localdata.R")))
  configuration = local_data(covid_uk_path, n_runs)
} else {

  library(reticulate) # Run Python commands within R

  # Determine Python binary from the current 'which python3' command result
  python_version <- system("which python3", intern=TRUE)
  use_python(python_version)

  # Import the StandardAPI from the SCRC data pipeline API
  api_py <- import("data_pipeline_api.standard_api")$StandardAPI

  # Fetch Git Metadata
  scrc = file.path(covid_uk_path, "SCRC")
  source(try_loc(file.path(scrc, "R", "Git.R")))

  # Create a function as a wrapper allowing direct usage of the API
  StandardAPI <- api_py$from_config(config_loc, GitMetadata$URL, GitMetadata$CommitSHA1)

  python_version <- system("which python3", intern=TRUE)
  if(python_version == 1)
  {
    stop("No Python3 installation detected via BASH command 'which python3'")
  }
  options_print_str = c(options_print_str, "\tSource : API\n")
  options_print_str = c(options_print_str, paste("\tConfig Path :", config_path, "\n"))
  options_print_str = c(options_print_str, paste("\tPython Path :", python_version))
  source(try_loc(file.path(scrc, "R", "remotedata.R")))
  source(try_loc(file.path(scrc, "R", "pushdata.R")))
  if(!file.exists(gsub("config.yaml", "metadata.yaml", config_path)))
  {
    cat(paste0("\n[Data Download]:\n\tNo 'metadata.yaml' at location '", 
               gsub("config.yaml", "", config_path),
               "' downloading data from API...\n\n"))
    system(paste(python_version, "-m data_pipeline_api.registry.download --config", config_path))
    cat("\n\tData download complete.\n")
  }
  configuration = remote_data(StandardAPI, n_runs)
}

options_print_str = c(options_print_str, configuration$output_str)

model_structures = build_params_from_args(configuration$params)

parameters = model_structures$parameters
options_print_str = c(options_print_str, "\n\tMode : ", configuration$params$run_mode)

observables = list(
  dynamics = data.table(),
  totals = data.table()
  )

# Run the Model
cat(options_print_str)

for (r in 1:n_runs) 
{
  message(paste0("\n==== Running Realisation: ", r, "/", n_runs, " ===="))
  R0 = configuration$params$R0s[r]

  # Ensure dynamics and totals get updated
  run_result = run_simulation(r, R0, configuration$params, model_structures, 
                              observables$dynamics, observables$totals, dump_params)
  observables$dynamics = run_result$dynamics
  observables$totals = run_result$totals

  if(dump_params)
  {
    quit()
  }

  assert("Failed to retrieve Dynamics from Run", length(observables$dynamics) > 0)
  assert("Failed to retrieve Totals from Run", length(observables$totals) > 0)
  if(run_result$run_code == 0)
  {
    break;
  }
}

cm_save(observables$totals, file.path(covid_uk_path, "output", paste0("run-", sub(" ", "-", configuration$params$run_mode),"-", r, "-totals.qs")));
cm_save(observables$dynamics, file.path(covid_uk_path, "output", paste0("run-", sub(" ", "-", configuration$params$run_mode), "-", r, "-dynamics.qs")));

if(!local)
{
  push_data(file.path(covid_uk_path, "output", 
                      paste0("run-", sub(" ", "-", configuration$params$run_mode), "-", r)), 
                      config_path, StandardAPI)
  StandardAPI$file_api$close()
}

print(Sys.time())