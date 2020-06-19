# - - - - - - - - - - - - - - - - - - - - - - - 
# UK model: load data and analyse scenarios
# - - - - - - - - - - - - - - - - - - - - - - - 

library(rlang)
library(stringr)
library(ini)
library(qs)

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

options_print_str = paste("COVID-UK Path: ", covid_uk_path)

if(local)
{
  source(file.path(covid_uk_path, "covidm", "R", "localdata.R"))
  data = local_data(covid_uk_path, location="UK | Epping Forest")
  param_file_search = grep('--parameters*', argv, value = TRUE)
  settings_file_search = grep('--settings*', argv, value = TRUE)
  contact_matrices_file_search = grep('--contact-matrices*', argv, value = TRUE)

  if(length(param_file_search) > 0)
  {
    parameter_file = strsplit(param_file_search, split = '=')[[1]][[2]];
  } else {
    parameter_file = file.path(covid_uk_path, 'configuration', 'parameters.ini');
  }

  if(length(settings_file_search) > 0)
  {
    settings_file = strsplit(settings_file_search, split = '=')[[1]][[2]];
  } else {
    settings_file = file.path(covid_uk_path, 'configuration', 'settings.ini');
  }

  if(length(contact_matrices_file_search) > 0)
  {
    contact_matrices_file = strsplit(contact_matrices_file_search, split = '=')[[1]][[2]];
  } else {
    contact_matrices_file = file.path(covid_uk_path, 'configuration', 'all_matrices.rds');
  }

  config_params   = read.ini(parameter_file)
  config_params$contact_matrix = data$contact_matrix
  config_params$population = list(count=data$population, label=data$labels)
  config_settings = read.ini(settings_file)

  options_print_str = c(options_print_str,paste("Using parameters From: ",parameter_file))
  options_print_str = c(options_print_str, paste("Using settings From: ",settings_file))
  options_print_str = c(options_print_str, paste("Reading Contact Matrices From: ", contact_matrices_file))

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

source(file.path(cm_path, "R", "BuildStructure.R"))
parameters = build_params_from_args(config_params, config_settings)