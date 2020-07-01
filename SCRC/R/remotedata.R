library(reticulate)
use_python("/home/kristian/venvs/lshtm/bin/python")

StandardAPI <- import("data_pipeline_api.standard_api")$StandardAPI

unpack_intervention = function(config_loc, ngroups)
{
    read_table = StandardAPI(config_loc)$read_table
    intervention = read_table("intervention_rates", "intervention_rates")

    return(
        list(
            contact = intervention[,1][1:9],
            fIs = rep(intervention[1,][10], ngroups)
        )
    )
}

unpack_terms = function(config_loc)
{
    read_table = StandardAPI(config_loc)$read_table

    pop_size = read_table("school_terms", "school_terms")

    return(
        list(
            close = pop_size[,2],
            reopen = pop_size[,2]
        )
    )

}

unpack_seeding = function(config_loc)
{
    read_estimate = StandardAPI(config_loc)$read_estimate

    return(
        list(
            value = read_estimate("seed", "seed"),
            min_age = read_estimate("min_age", "min_age"),
            max_age = read_estimate("max_age", "max_age"),
            seeding_start_range = read_estimate("seeding_min_start_day", "seeding_min_start_day") : 
                                  read_estimate("seeding_max_start_day", "seeding_max_start_day")
        )
    ) 
}

unpack_populations = function(config_loc)
{
    read_table = StandardAPI(config_loc)$read_table

    pop_size = read_table("population_sizes", "population_sizes")

    return(
        list(
            region = pop_size[,1],
            sample = pop_size[,2]
        )
    )

}

unpack_matrices = function(config_loc)
{
    read_array = StandardAPI(config_loc)$read_array

    # Model relies on two sets of matrices, the 'region'
    # set e.g. [1] Scotland, [2] UK and the 'sample' set 
    # set of that region e.g. [1] Scot. Health Board, [2] Scotland

    set_names = c("region", "sample")
    matrix_names = c("home", "other", "school", "work")

    contact_matrices = list()

    for(set in set_names)
    {
        contact_matrices[[set]] = list()
        for(name in matrix_names)
        {
            contact_matrices[[set]][[name]] = read_array("contact_matrices", file.path("contact_matrices", set, name))
        }
    }
    return(contact_matrices)
}

objects = function(config_loc)
{
    read_table = StandardAPI(config_loc)$read_table
    read_array = StandardAPI(config_loc)$read_array

    params = list(
            age_var_symptom_rates = read_table("age_var_symptomatic_rates", "age_varying_symptomatic_rates"),
            health_burden_probabilities = read_table("health_burden_processes", "health_burden_processes"),
            contact_matrices = unpack_matrices(config_loc),
            lockdown_rates = read_table("lockdown_rates", "lockdown_rates"),
            school_holiday_rates = read_table("school_holiday_rates", "school_holiday_rates"),
            size = unpack_populations(config_loc),
            school_terms = unpack_terms(config_loc),
            seed = unpack_seeding(config_loc)
        )
    n_groups = params$contact_matrices$region %>% .$other %>% colnames %>% length
    params$intervention = unpack_intervention(config_loc, n_groups)

    return(params)
}

remote_data = function(covid_uk)
{
    output_str = ""
    # Define fixed parameter choices (those that do not need to be set by the API itself)
    config_params = list(
        child_grandparentcontacts = FALSE,  # This is analysis specific so should be switched off
                                            # to ensure flexibility
        deterministic = FALSE,              # Seed the age distributions (set to False in "vanilla" run UK.R)
        mode = "Normal"                     # Normal analysis run as opposed "R0 Analysis" which checks
                                            # intervention effect on R0 (might be needed later as a separate run?)
    )

    config_loc = file.path(covid_uk, "SCRC", "pipeline_data", "config.yaml")

    config_params = append(config_params, objects(config_loc))

    return(list(params=config_params, output_str=options_print_str))
}