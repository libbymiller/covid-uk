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
if (argc == 3) {
    option.single = as.numeric(argv[argc-2]);
} else {
    option.single = -1;
}

out_file = file("current_params.txt")

# List setup of files in one print statement

local_run = grep('--local', argv, value = FALSE)

local = length(local_run) > 0

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
source(file.path(cm_path, "R", "covidm.R"))


analysis = as.numeric(argv[1]);
n_runs = as.numeric(argv[2]);

# Set path
# Set this path to the base directory of the repository.
# NOTE: Run from repository

# build parameters for entire UK, for setting R0.

source(file.path(cm_path, "R", "BuildStructures.R"))

parameters = build_params_from_args(analysis, configuration$parameters,
                                    configuration$settings)

print(parameters$pop)