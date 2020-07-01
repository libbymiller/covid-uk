library(reticulate)
use_python("/home/kristian/venvs/lshtm/bin/python")

StandardAPI <- import("data_pipeline_api.standard_api")$StandardAPI

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

    return(
        list(
            age_var_symptom_rates = read_table("age_var_symptomatic_rates", "age_varying_symptomatic_rates"),
            health_burden_probabilities = read_table("health_burden_processes", "health_burden_processes"),
            contact_matrices = unpack_matrices(config_loc),
            lockdown_rates = read_table("lockdown_rates", "lockdown_rates"),
            school_term_rates = read_table("school_term_rates", "school_term_rates")
        )
    )
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

    print(config_params$school_term_rates)
    stop()

    return(list(params=config_params, output_str=options_print_str))
}