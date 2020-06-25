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

source(file.path(cm_path, "R", "covidm.R"))
source(file.path(cm_path, "R", "Utilities.R"))

if(rebuild)
{
  message("Rebuilding C++ Libraries")
  rebuild_cpp_libraries(cm_path)
}
options_print_str = "[Configuration]:\n"
if(local)
{
  options_print_str = c(options_print_str, "\tSource : Local\n")
  source(try_loc(file.path(covid_uk_path, "covidm", "R", "localdata.R")))
  configuration = local_data(covid_uk_path)
}

# Set path
# Set this path to the base directory of the repository.
# NOTE: Run from repository

# build parameters for entire UK, for setting R0.

source(try_loc(file.path(cm_path, "R", "BuildStructures.R")))
source(try_loc(file.path(cm_path, "R", "Simulate.R")))
source(try_loc("covidm/build/sourceCpp-x86_64-pc-linux-gnu-1.0.4.6/sourcecpp_5d441d22b081/corona.cpp.R"))

options_print_str = c(options_print_str, configuration$output_str)

set.seed(as.numeric(configuration$params$seed$value))

model_structures = build_params_from_args(configuration$params)
parameters = model_structures$parameters

R0s = rnorm(n_runs, mean = as.numeric(configuration$params$r0_distribution$mean),
            sd = as.numeric(configuration$params$r0_distribution$sd))[[1]]

dynamics = data.table()
totals = data.table()

output_file_name = file.path(covid_uk_path, "dynamics.qs")
options_print_str = c(options_print_str, paste("\n\tOutput File:", output_file_name, "\n"))

# Have to unpack these variables else Rcpp breaks (they are needed globally)
# FIXME: Would be good to have this global import external
uk_pop_struct = model_structures$uk_population_structure
locations = uk_pop_struct$locations
london = uk_pop_struct$london
england = uk_pop_struct$england
wales = uk_pop_struct$wales
scotland = uk_pop_struct$scotland
nireland = uk_pop_struct$nireland
westmid = uk_pop_struct$westmid
cumbria  = uk_pop_struct$cumbria

# Run the Model
cat(options_print_str)

for (r in 1:n_runs) 
{
  message(paste0("\n==== Running Realisation: ", r, "/", n_runs, " ===="))
  R0 = R0s[r]

  run_simulation(r, R0, configuration$params, model_structures, dump_params)
}

cm_save(totals, file.path(covid_uk_path, paste0(analysis, "-totals.qs")));
cm_save(dynamics, file.path(covid_uk_path, paste0(analysis, "-dynamics.qs")));
print(Sys.time())