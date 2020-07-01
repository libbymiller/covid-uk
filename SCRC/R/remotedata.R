library(reticulate)
use_python("/home/kristian/venvs/lshtm/bin/python")

StandardAPI <- import("data_pipeline_api.standard_api")$StandardAPI

objects = function(config_loc)
{
    read_table = StandardAPI(config_loc)$read_table

    return(
        list(
            age_var_symptom_rates = read_table("age_var_symptomatic_rates", "age_varying_symptomatic_rates"),
            health_burden_probabilities = read_table("health_burden_processes", "health_burden_processes")
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

    return(list(params=config_params, output_str=options_print_str))
}