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

unpack_times = function(config_loc)
{
    read_estimate = StandardAPI(config_loc)$read_estimate

    return(
        list(
            max = read_estimate("delay_gamma", "max_day_delay_gamma"),
            step = read_estimate("delay_gamma", "time_step_delay_gamma"),
            end = read_estimate("end_day", "end_day"),
            start = read_estimate("start_day", "start_day"),
            start_date = as.Date(read_estimate("start_date_posix", "start_date_posix"), "1970-01-01")
        )
    )
}

unpack_dis_params = function(config_loc)
{
    read_estimate = StandardAPI(config_loc)$read_estimate

    compartments = c("dE", "dIp", "dIa", "dIs")

    params = list()

    for(comp in compartments)
    {
        params[[comp]] = list(mu=read_estimate(file.path("delay_gamma", comp), "mu"),
                             shape=read_estimate(file.path("delay_gamma", comp), "shape")
        )
    }

    params[["delay_Ip_to_hosp"]] = list(mu=read_estimate(file.path("delay_gamma", "ip_to_hosp"), "mu"),
                                        shape=read_estimate(file.path("delay_gamma", "ip_to_hosp"), "shape")
                                        )
    params[["delay_to_icu"]] = list(mu=read_estimate(file.path("delay_gamma", "to_icu"), "mu"),
                                        shape=read_estimate(file.path("delay_gamma", "to_icu"), "shape")
                                        )
    params[["delay_to_non_icu"]] = list(mu=read_estimate(file.path("delay_gamma", "to_non_icu"), "mu"),
                                        shape=read_estimate(file.path("delay_gamma", "to_non_icu"), "shape")
                                        )
    params[["delay_Ip_to_death"]] = list(mu=read_estimate(file.path("delay_gamma", "ip_to_death"), "mu"),
                                        shape=read_estimate(file.path("delay_gamma", "ip_to_death"), "shape")
                                        )

    return(params)
}

unpack_trigger = function(config_loc)
{
    read_estimate = StandardAPI(config_loc)$read_estimate

    return(
        list(
            trigger = ifelse(read_estimate("isnational", "isnational") == 0, "national", "local"),
            duration = read_estimate("duration", "duration"),
            icu_bed_usage = read_estimate("icu_bed_usage", "icu_bed_usage"),
            intervention_shift = read_estimate("intervention_shift", "intervention_shift")
        )
    )
}

create_R0s = function(config_loc, seed, n)
{
    np_rand <- import("numpy")$random
    np_rand$seed(seed)

    read_distribution = StandardAPI(config_loc)$read_distribution

    norm = read_distribution("r0_distribution", "r0_distribution")

    return(norm$rvs(as.integer(n)))
}

objects = function(config_loc, n_groups)
{
    read_table = StandardAPI(config_loc)$read_table
    read_array = StandardAPI(config_loc)$read_array
    read_estimate = StandardAPI(config_loc)$read_estimate

    params = list(
            age_var_symptom_rates = read_table("age_var_symptomatic_rates", "age_varying_symptomatic_rates"),
            health_burden_probabilities = read_table("health_burden_processes", "health_burden_processes"),
            contact_matrices = unpack_matrices(config_loc),
            lockdown_rates = read_table("lockdown_rates", "lockdown_rates"),
            school_holiday_rates = read_table("school_holiday_rates", "school_holiday_rates"),
            size = unpack_populations(config_loc),
            school_terms = unpack_terms(config_loc),
            seed = unpack_seeding(config_loc),
            fIs = read_estimate("rel_symptomatic", "rel_symptomatic"),
            fIp = read_estimate("rel_preclinical", "rel_preclinical"),
            fIa = read_estimate("rel_subclinical", "rel_subclinical"),
            time = unpack_times(config_loc),
            lockdown_trigger = unpack_trigger(config_loc)
        )
    params = append(params, unpack_dis_params(config_loc))
    params$R0s = create_R0s(config_loc, params$seed$value, n_groups)
    n_groups = params$contact_matrices$region %>% .$other %>% colnames %>% length
    params$intervention = unpack_intervention(config_loc, n_groups)

    return(params)
}

remote_data = function(covid_uk, n_groups)
{
    output_str = ""
    # Define fixed parameter choices (those that do not need to be set by the API itself)
    config_params = list(
        child_grandparentcontacts = FALSE,  # This is analysis specific so should be switched off
                                            # to ensure flexibility
        deterministic = FALSE,              # Seed the age distributions (set to False in "vanilla" run UK.R)
        mode = "Normal",                    # Normal analysis run as opposed "R0 Analysis" which checks
                                            # intervention effect on R0 (might be needed later as a separate run?)
        elderly_from_bin = 15,              # Bin which defines point at which individual is classed as elderly (set to '65-70')
        dH = 1, dC = 1                      # Unused by model at the moment
    )

    config_loc = file.path(covid_uk, "SCRC", "pipeline_data", "config.yaml")

    config_params = append(config_params, objects(config_loc, n_groups))

    return(list(params=config_params, output_str=options_print_str))
}