suppressPackageStartupMessages({
    library(qs)
    library(readxl)
    library(ini)
    library(rlang)
    library(stringr)
    library(tidyverse)
})

local_data_files = list(
    parameter_file = file.path('configuration', 'parameters.ini'),
    interventions_file = file.path('configuration', 'interventions.ini'),
    contact_matrices_file = file.path('data', 'all_matrices.rds'),
    age_var_symptom_rates = file.path("data", "2-linelist_symp_fit_fIa0.5.qs"),
    uk_population = file.path("data", "wpp2019_pop2020.rds"),
    uk_structure = file.path("data", "structure_UK.rds"),
    health_burden_process_data = file.path('data', "health_burden_processes.csv"),
    school_terms = file.path("data", "school_terms_base.csv")
)
# Get regions for the UK.
cm_uk_locations = function(arguments, country, level) {
    # Check country code
    country = toupper(country);
    if (country == "UK") { 
        country = "EWSN";
    }
    if (!country %like% "^(UK|[EWSN]+)$") {
        stop("country must be UK, or a combination of E, W, S, and/or N.");
    }
    
    # Interpret level
    level = as.integer(level);
    if (level < 0 | level > 4) {
        stop("level must be 0, 1, 2, 3, or 4");
    }
    
    if (level == 0) {
        if (country != "EWSN") {
            stop("For level 0, country must be UK.");
        }
        return ("UK | UNITED KINGDOM");
    } else if (level == 1) {
        gE = "Country";
        gW = "Country";
        gS = "Country";
        gN = "Country";
    } else if (level == 2) {
        gE = "Region";
        gW = "Country";
        gS = "Country";
        gN = "Country";
    } else if (level == 3) {
        gE = c("Metropolitan County", "County", "Unitary Authority", "London Borough");
        gW = "Unitary Authority";
        gS = "Council Area";
        gN = "Local Government District";
    } else if (level == 4) {
        gE = c("Metropolitan District", "Non-metropolitan District", "Unitary Authority", "London Borough");
        gW = "Unitary Authority";
        gS = "Council Area";
        gN = "Local Government District";
    }
    
    # Extract locations
    locs = NULL;
    if (country %like% "E") { locs = c(locs, arguments$uk_structure[Code %like% "^E" & Geography1 %in% gE, Name]); }
    if (country %like% "W") { locs = c(locs, arguments$uk_structure[Code %like% "^W" & Geography1 %in% gW, Name]); }
    if (country %like% "S") { locs = c(locs, arguments$uk_structure[Code %like% "^S" & Geography1 %in% gS, Name]); }
    if (country %like% "N") { locs = c(locs, arguments$uk_structure[Code %like% "^N" & Geography1 %in% gN, Name]); }
    return (paste0("UK | ", locs));
}

local_data = function(covid_dir)
{
    options_print_str = c(paste("\tCOVID-UK Path : ", covid_dir))
    options_print_str = c(options_print_str, paste("\n\tParameter File : ", local_data_files$parameter_file))
    options_print_str = c(options_print_str, paste("\n\tMatrices File : ", local_data_files$contact_matrices_file))
    options_print_str = c(options_print_str, paste("\n\tAge-Varying Symptomatic Rates File : ", local_data_files$age_var_symptom_rates))
    options_print_str = c(options_print_str, paste("\n\tSchool Terms Data File : ", local_data_files$school_terms))

    config_params   = read.ini(file.path(covid_dir, local_data_files$parameter_file))

    config_params$school_holiday_rates = c(config_params$school_holiday_rates$home,
                                        config_params$school_holiday_rates$work,
                                        config_params$school_holiday_rates$schools,
                                        config_params$school_holiday_rates$other,
                                        config_params$school_holiday_rates$home_elderly,
                                        config_params$school_holiday_rates$work_elderly,
                                        config_params$school_holiday_rates$schools_elderly,
                                        config_params$school_holiday_rates$other_elderly,
                                        config_params$school_holiday_rates$child_elderly)

    config_params$lockdown_rates = c(config_params$lockdown_rates$home,
                                        config_params$lockdown_rates$work,
                                        config_params$lockdown_rates$schools,
                                        config_params$lockdown_rates$other,
                                        config_params$lockdown_rates$home_elderly,
                                        config_params$lockdown_rates$work_elderly,
                                        config_params$lockdown_rates$schools_elderly,
                                        config_params$lockdown_rates$other_elderly,
                                        config_params$lockdown_rates$child_elderly)

    config_params$population = readRDS(file.path(covid_dir, local_data_files$uk_population))

    # Get number of bins from dividing the minimum highest bin (i.e. labelled "X+") across all UK regions
    # and dividing by 5 years
    ngroups_from_pop_dat = config_params$population %>% .[.$name %like% "UK", ] %>% .[.$age %like% "\\+", ]
    ngroups_from_pop_dat = min(as.numeric(sub("\\+", "", ngroups_from_pop_dat$age)))/5

    # Get seeding start day from provided possible range
    # using list keeps possibility of seeding more than one population

    config_params$seed$seeding_start_range = eval(parse(text=config_params$seed$seeding_start_range))

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

    # Setup region name for producing parameters to set R0 and
    # sample name for the parameters on which to run the model
    # in this example: 158 - "Glasgow City"
    config_params$region_name  = cm_uk_locations(config_params, "UK", 0)
    config_params$sample_name  = cm_uk_locations(config_params, "UK", 3)[[158]]

    # Read the contact matrices
    contact_matrices = readRDS(file.path(covid_dir, local_data_files$contact_matrices_file))
    config_params$contact_matrices$region = contact_matrices[[config_params$region_name]]
    config_params$contact_matrices$sample = contact_matrices[[config_params$sample_name]]
  
    # Determine the minimum number of age groups present in this data
    n_groups_cm = config_params$contact_matrices$region %>% .$other %>% colnames %>% length
    config_params$ngroups = min(ngroups_from_pop_dat, n_groups_cm)

    # Get Population sizes for region and sample
    demographics_regional = cm_get_demographics(config_params$region_name, config_params$ngroups);

    config_params$size = list()

    config_params$size$region = demographics_regional[, round((f + m) * 1000)];

    demographics_sample = cm_get_demographics(config_params$sample_name, config_params$ngroups)
    config_params$size$sample = demographics_sample[, round((f + m) * 1000)];
 
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
                                    fIs = rep(as.numeric(int_par$school_closures$fIs_perage), config_params$ngroups)
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
                                    fIs = rep(as.numeric(int_par$social_distancing$fIs_perage), config_params$ngroups)
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
                                    fIs = rep(as.numeric(int_par$elderly_shielding$fIs_perage), config_params$ngroups)
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
                                    fIs = rep(as.numeric(int_par$self_isolation$fIs_perage), config_params$ngroups)
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
                                    fIs = rep(as.numeric(int_par$combination$fIs_perage), config_params$ngroups)
        )
    )

    if(config_params$run_mode$mode != "R0 Analysis")
    {
        config_params$intervention = interventions[[config_params$intervention_preset$name]]
        options_print_str = c(options_print_str, paste("\n\tInterventions Preset: ", config_params$intervention_preset$name))
    }
    else
    {
        config_params$intervention = interventions
    }

    return(list(params=config_params, output_str=options_print_str))
}