library(qs)
library(readxl)
library(ini)
library(rlang)
library(stringr)

local_data_files = list(
    parameter_file = file.path('configuration', 'parameters.ini'),
    settings_file = file.path('configuration', 'settings.ini'),
    contact_matrices = file.path('configuration', 'all_matrices.rds'),
    age_var_symptom_rates = file.path("data", "2-linelist_symp_fit_fIa0.5.qs"),
    uk_population = list(address = file.path('data', 'ukmidyearestimates20192020ladcodes.xls'),
                         sheet = "MYE1",
                         range = "A12:B31"
    ),
    health_burden_process_data = file.path('data', "health_burden_processes.csv")
)

local_data = function(covid_dir, location)
{
    options_print_str = c(paste("COVID-UK Path: ", covid_dir))
    options_print_str = c(options_print_str, paste("Using parameters From: ", local_data_files$parameter_file))
    options_print_str = c(options_print_str, paste("Using settings From: ", local_data_files$settings_file))
    options_print_str = c(options_print_str, paste("Reading Contact Matrices From: ", local_data_files$contact_matrices_file))
    options_print_str = c(options_print_str, paste("Loading Age-Varying Symptomatic Rate From:", local_data_files$age_var_symptom_rates))

    config_params   = read.ini(file.path(covid_dir, local_data_files$parameter_file))
    config_settings = read.ini(file.path(covid_dir, local_data_files$settings_file))

    # Read contact matrices
    cm = readRDS(file.path(covid_dir, local_data_files$contact_matrices))
    cm = cm[[location]]
    cm = list(home=cm[["home"]], work=cm[["work"]], school=cm[["other"]],
            other=cm[["other"]])

    config_params$contact_matrices = cm

    population = read_xls(file.path(covid_dir, local_data_files$uk_population$address), 
                          sheet=local_data_files$uk_population$sheet, range=local_data_files$uk_population$range)

    # Convert age bins to match contact matrices, i.e. 75+ is last bin not 100+
    corr_pop = population[["...2"]][-c(17,18,19)]
    corr_pop[[16]] = population[["...2"]][16]+population[["...2"]][17]+population[["...2"]][18]+population[["...2"]][19]
    config_params$population = list(count=corr_pop, label=colnames(cm$home))


    # Load Age Varying Symptomatic Rates from Prior Analysis
    covid_scenario = qread(local_data_files$age_var_symptom_rates)
    config_params$age_var_symptom_rates = covid_scenario

    # Health burden processes
    probs = fread(file.path(covid_dir, local_data_files$health_burden_process_data))
    config_params$health_burden_probabilities = probs

    return(list(parameters=config_params, settings=config_settings))
}