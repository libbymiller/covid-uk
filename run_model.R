# - - - - - - - - - - - - - - - - - - - - - - - 
# UK model: load data and analyse scenarios
# - - - - - - - - - - - - - - - - - - - - - - - 

library(rlang)
library(stringr)
library(ini)
library(qs)
library(data.table)

# Load requested settings from command line
argv = commandArgs(trailingOnly = TRUE);
argc = length(argv);

out_file = file("current_params.txt")

# List setup of files in one print statement

local_run = grep('--local', argv, value = FALSE)
dump_params = grep('--dump', argv, value = FALSE) # Dump parameters prior to run and exit (for testing)

local = length(local_run) > 0
dump_params = length(dump_params) > 0

covid_uk_search = grep('--covid-uk-path*', argv, value = TRUE)
if(length(covid_uk_search) > 0)
{
  covid_uk_path = strsplit(covid_uk_search, split = '=')[[1]][[2]];
} else {
  covid_uk_path = getwd();
}


if(local)
{
  source(file.path(covid_uk_path, "covidm", "R", "localdata.R"))
  configuration = local_data(covid_uk_path, location="UK | Epping Forest")
}


# covidm options
cm_path = file.path(covid_uk_path, "covidm");

# Set path
# Set this path to the base directory of the repository.
# NOTE: Run from repository

# build parameters for entire UK, for setting R0.

source(file.path(cm_path, "R", "BuildStructures.R"))
source(file.path(cm_path, "R", "Utilities.R"))
source(file.path(cm_path, "R", "Simulate.R"))

options_print_str = configuration$output_str

set.seed(as.numeric(configuration$params$seed$value))

model_structures = build_params_from_args(configuration$params)
parameters = model_structures$parameters



R0 = rnorm(1, mean = as.numeric(configuration$params$r0_distribution$mean),
            sd = as.numeric(configuration$params$r0_distribution$sd))[[1]]

dynamics = data.table()
totals = data.table()

output_file_name = file.path(covid_uk_path, "dynamics.qs")
options_print_str = c(options_print_str, paste("Output File:", output_file_name))


# Run the Model
options_print_str = c(options_print_str, paste0("R0 = ", R0))
print(options_print_str)

parameters = pre_simulation_setup(R0, configuration$params, model_structures)

if(dump_params)
{
  output_file = file.path(covid_uk_path, "output", paste0("params-", gsub(" ", "", gsub(":","",Sys.time())), ".pars"))
  dput(parameters, file=output_file)
  message(paste0("Params saved to '", output_file,"' aborting"))
  return(0)
}

run_simulation(R0, configuration$params, parameters)