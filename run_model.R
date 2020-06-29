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

local = length(local_run) > 0
dump_params = length(dump_params) > 0
rebuild = length(rebuild) > 0

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
  configuration = local_data(covid_uk_path)
}

options_print_str = c(options_print_str, configuration$output_str)

set.seed(as.numeric(configuration$params$seed$value))

options_print_str = c(options_print_str, "\n\tSeed : ", as.numeric(configuration$params$seed$value))

model_structures = build_params_from_args(configuration$params)
parameters = model_structures$parameters
options_print_str = c(options_print_str, "\n\tMode : ", configuration$params$run_mode$mode)

R0s = rnorm(n_runs, mean = as.numeric(configuration$params$r0_distribution$mean),
            sd = as.numeric(configuration$params$r0_distribution$sd))

observables = list(
  dynamics = data.table(),
  totals = data.table()
  )

# Run the Model
cat(options_print_str)

for (r in 1:n_runs) 
{
  message(paste0("\n==== Running Realisation: ", r, "/", n_runs, " ===="))
  R0 = R0s[r]

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

cm_save(observables$totals, file.path(covid_uk_path, "output", paste0("run-", sub(" ", "-", configuration$params$run_mode$mode),"-", r, "-totals.qs")));
cm_save(observables$dynamics, file.path(covid_uk_path, "output", paste0("run-", sub(" ", "-", configuration$params$run_mode$mode), "-", r, "-dynamics.qs")));
print(Sys.time())