library(qs)
library(readxl)
library(ini)
library(rlang)
library(stringr)

local_data_files = list(
    parameter_file = file.path('configuration', 'parameters.ini'),
    settings_file = file.path('configuration', 'settings.ini'),
    interventions_file = file.path('configuration', 'interventions.ini'),
    contact_matrices_file = file.path('data', 'all_matrices.rds'),
    age_var_symptom_rates = file.path("data", "2-linelist_symp_fit_fIa0.5.qs"),
    uk_population = list(address = file.path('data', 'ukmidyearestimates20192020ladcodes.xls'),
                         sheet = "MYE1",
                         range = "A12:B31"
    ),
    uk_structure = file.path("data", "structure_UK.rds"),
    health_burden_process_data = file.path('data', "health_burden_processes.csv"),
    school_terms = file.path("data", "school_terms_base.csv")
)

local_data = function(covid_dir)
{
    options_print_str = c(paste("COVID-UK Path: ", covid_dir))
    options_print_str = c(options_print_str, paste("Using parameters From: ", local_data_files$parameter_file))
    options_print_str = c(options_print_str, paste("Using settings From: ", local_data_files$settings_file))
    options_print_str = c(options_print_str, paste("Reading Contact Matrices From: ", local_data_files$contact_matrices_file))
    options_print_str = c(options_print_str, paste("Loading Age-Varying Symptomatic Rate From:", local_data_files$age_var_symptom_rates))
    options_print_str = c(options_print_str, paste("Reading School Terms Base Data From:", local_data_files$school_terms_base_file))

    config_params   = read.ini(file.path(covid_dir, local_data_files$parameter_file))

    # Read contact matrices
    config_params$contact_matrices = readRDS(file.path(covid_dir, local_data_files$contact_matrices_file))

    population = read_xls(file.path(covid_dir, local_data_files$uk_population$address), 
                          sheet=local_data_files$uk_population$sheet, range=local_data_files$uk_population$range)

    # Convert age bins to match contact matrices, i.e. 75+ is last bin not 100+
    corr_pop = population[["...2"]][-c(17,18,19)]
    corr_pop[[16]] = population[["...2"]][16]+population[["...2"]][17]+population[["...2"]][18]+population[["...2"]][19]

    # FIXME: So happens contact matrix 2 has the correct labels for the names of the columns
    # this very likely will not be the case every time!
    config_params$population = list(count=corr_pop, label=colnames(config_params$contact_matrices[[2]]$home))

    ngroups = length(config_params$population$label)

    if(ngroups != 16)
    {
        
        stop("Resizing of local data failed")
    }

    # Load Age Varying Symptomatic Rates from Prior Analysis
    config_params$age_var_symptom_rates = qread(local_data_files$age_var_symptom_rates)

    # Health burden processes
    probs = fread(file.path(covid_dir, local_data_files$health_burden_process_data))
    config_params$health_burden_probabilities = probs

    # Load UK Structure
    config_params$uk_structure = readRDS(file.path(covid_dir, local_data_files$uk_structure))

    # Define school terms
    school_terms = read.csv(local_data_files$school_terms)
    config_params$school_terms = list(close = school_terms[, 1], reopen=school_terms[, 2])

    # Define interventions to be used
    int_par = read.ini(local_data_files$interventions_file)

    interventions = list(
        `School Closures`   = list(contact = c(as.numeric(int_par$school_closures$home), 
                                               as.numeric(int_par$school_closures$work),
                                               as.numeric(int_par$school_closures$schools),
                                               as.numeric(int_par$school_closures$other),
                                               as.numeric(int_par$school_closures$home_elderly),
                                               as.numeric(int_par$school_closures$work_elderly),
                                               as.numeric(int_par$school_closures$schools_elderly),
                                               as.numeric(int_par$school_closures$other_elderly),
                                               as.numeric(int_par$school_closures$child_elderly)),
                                    fIs = rep(as.numeric(int_par$school_closures$fIs_perage), ngroups)
        ),

        `Social Distancing` = list(contact = c(as.numeric(int_par$social_distancing$home), 
                                               as.numeric(int_par$social_distancing$work),
                                               as.numeric(int_par$social_distancing$schools),
                                               as.numeric(int_par$social_distancing$other),
                                               as.numeric(int_par$social_distancing$home_elderly),
                                               as.numeric(int_par$social_distancing$work_elderly),
                                               as.numeric(int_par$social_distancing$schools_elderly),
                                               as.numeric(int_par$social_distancing$other_elderly),
                                               as.numeric(int_par$social_distancing$child_elderly)),
                                    fIs = rep(as.numeric(int_par$social_distancing$fIs_perage), ngroups)
        ),

        `Elderly Shielding` = list(contact = c(as.numeric(int_par$elderly_shielding$home), 
                                               as.numeric(int_par$elderly_shielding$work),
                                               as.numeric(int_par$elderly_shielding$schools),
                                               as.numeric(int_par$elderly_shielding$other),
                                               as.numeric(int_par$elderly_shielding$home_elderly),
                                               as.numeric(int_par$elderly_shielding$work_elderly),
                                               as.numeric(int_par$elderly_shielding$schools_elderly),
                                               as.numeric(int_par$elderly_shielding$other_elderly),
                                               as.numeric(int_par$elderly_shielding$child_elderly)),
                                    fIs = rep(as.numeric(int_par$elderly_shielding$fIs_perage), ngroups)
        ),

        `Self-Isolation`    = list(contact = c(as.numeric(int_par$self_isolation$home), 
                                               as.numeric(int_par$self_isolation$work),
                                               as.numeric(int_par$self_isolation$schools),
                                               as.numeric(int_par$self_isolation$other),
                                               as.numeric(int_par$self_isolation$home_elderly),
                                               as.numeric(int_par$self_isolation$work_elderly),
                                               as.numeric(int_par$self_isolation$schools_elderly),
                                               as.numeric(int_par$self_isolation$other_elderly),
                                               as.numeric(int_par$self_isolation$child_elderly)),
                                    fIs = rep(as.numeric(int_par$self_isolation$fIs_perage), ngroups)
        ),
                                    
        `Combination`       = list(contact = c(as.numeric(int_par$combination$home), 
                                               as.numeric(int_par$combination$work),
                                               as.numeric(int_par$combination$schools),
                                               as.numeric(int_par$combination$other),
                                               as.numeric(int_par$combination$home_elderly),
                                               as.numeric(int_par$combination$work_elderly),
                                               as.numeric(int_par$combination$schools_elderly),
                                               as.numeric(int_par$combination$other_elderly),
                                               as.numeric(int_par$combination$child_elderly)),
                                    fIs = rep(as.numeric(int_par$combination$fIs_perage), ngroups)
        )
    )

    config_params$intervention = interventions[[config_params$intervention$type]]

    return(list(params=config_params, output_str=options_print_str))
}