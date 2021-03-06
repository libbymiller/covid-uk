##############################################################################
#                                                                            #
#                      API Based Argument Construction                       #
#                                                                            #
#   Construct argument set using the arguments from as defined by the SCRC   #
#   data API implementation. Parameter values and data are read from files   #
#   using the API, these files can therefore be dynamically changed.         #
#                                                                            #
#   @author : K. Zarebski (UKAEA)                                            #
#   @date   : last modified 2020-08-27                                       #
#                                                                            #
##############################################################################

library(reticulate) # Run Python commands within R

py_time <- import("time")$time

#' Get the intervention
#' 
#' Retrieve the current intervention choice from the API
#' 
#' @param standard_api Instance of standard API object
#' @param ngroups Number of age groups
unpack_intervention <- function(standard_api, ngroups)
{
    read_table <- standard_api$read_table
    intervention <- read_table("interventions/intervention_rates", "intervention_rates")

    # Rates associated with the given intervention are defined as
    # 9 contact rates for the contact matrix types and a factor on the
    # symptomatic infectiousness
    return(
        list(
            contact = intervention[1:9] %>% unlist,
            fIs = rep(intervention[[10]], ngroups)
        )
    )
}

#' Get School Terms
#' 
#' Retrieve the school terms from the API
#' 
#' @param standard_api Instance of standard API object
unpack_terms <- function(standard_api)
{
    read_table <- standard_api$read_table
    school_terms <- read_table("school/school_terms", "school_terms")

    return(
        list(
            close = school_terms[[1]],
            reopen = school_terms[[2]]
        )
    )

}

#' Get Seed Settings
#' 
#' Retrieves the seeding configuration from the relevant
#' files via the API
#' 
#' @param standard_api Instance of standard API object
unpack_seeding <- function(standard_api)
{
    read_estimate <- standard_api$read_estimate

    return(
        list(
            value = read_estimate("run-configuration/seeding", "seed"),
            min_age = read_estimate("run-configuration/seeding", "min_age"),
            max_age = read_estimate("run-configuration/seeding", "max_age"),
            seeding_start_range = read_estimate("run-configuration/seeding", "seeding_min_start_day") : 
                                  read_estimate("run-configuration/seeding", "seeding_max_start_day")
        )
    ) 
}

#' Fetch populations
#' 
#' Retrieve the populations for the given regions.
#' FIXME: Assume in future these two data sets will be separate
#' UNITED KINGDOM set should be read as is, but "subset" set can be
#' any other data set in the same form (eg. health board in same binning)
#' or specify a different region below
#' 
#' @param standard_api Instance of standard API object
#' @param region Name of the subset region, default is the demo of Glasgow City
unpack_populations <- function(standard_api, region="Glasgow City")
{

    read_table <- standard_api$read_table
    pop_size <- read_table("population/population_sizes", "population_size/persons")

    return(
        list(
            region = pop_size[which(pop_size$Name == "UNITED KINGDOM"), ] %>% select(., -Name) %>% as.vector %>% as.numeric,
            subset = pop_size[which(pop_size$Name %like% region), ] %>% select(., -Name) %>% as.vector %>% as.numeric
        )
    )

}

#' Get Contact Matrices
#' 
#' Retrieve the contact matrices from the API
#' 
#' @param standard_api Instance of standard API object
unpack_matrices <- function(standard_api)
{
    read_array <- standard_api$read_array

    # Model relies on two sets of matrices, the 'region'
    # set e.g. [1] Scotland, [2] UK and the 'subset' set 
    # set of that region e.g. [1] Scot. Health Board, [2] Scotland

    matrix_names <- c("home", "other", "school", "work")

    contact_matrices <- list()

    contact_matrices[["subset"]] <- list()
    for(name in matrix_names)
    {
        Array <- read_array("contact_matrices/subregion", file.path("contact_matrices", name))
        contact_matrices[["subset"]][[name]] = Array$data
    }

    contact_matrices[["region"]] = list()
    for(name in matrix_names)
    {
        Array <- read_array("contact_matrices/national", file.path("contact_matrices", name))
        contact_matrices[["region"]][[name]] = Array$data
    }
    return(list(matrices=contact_matrices, group_names=Array$dimensions[[1]]$names))
}

#' Get Time configurations
#' 
#' Retrieves the time based settings from the API
#'
#' @param standard_api Instance of standard API object
unpack_times <- function(standard_api)
{
    read_estimate <- standard_api$read_estimate

    return(
        list(
            max = read_estimate("fixed-parameters/delay_gamma", "max_delay_gamma"),
            step = read_estimate("fixed-parameters/delay_gamma", "time_step_delay_gamma"),
            end = read_estimate("run-configuration/time", "end_day"),
            start = read_estimate("run-configuration/time", "start_day"),
            start_date = as.Date(read_estimate("run-configuration/time", "start_date_posix"), "1970-01-01")
        )
    )
}

#' Get Distribution Components
#' 
#' For a given gamma distribution held within the API
#' the function returns the components from the args
#' and kwds members of the scipy.stats object
#' 
#' @param dis_label Name of the distribution in the API
#' @param standard_api Instance of standard API object

fetch_gamma_components <- function(dis_label, standard_api)
{
    distribution <- standard_api$read_distribution(file.path("distributions", dis_label), dis_label)
    
    return(
        list(
            distribution$args[[1]],
            distribution$kwds[["scale"]]
        )
    )
}

#' Get and Store Distribution Parameters
#' 
#' Unpacks the distribution parameters from the API
#' and stores them into a list matching the form
#' of the local run arguments
#' 
#' @param standard_api Instance of standard API object
unpack_dis_params <- function(standard_api)
{

    compartments <- c("dE", "dIp", "dIa", "dIs")

    params <- list()

    for(comp in compartments)
    {
        args <- fetch_gamma_components(comp, standard_api)

        params[[comp]] = list(mu=args[[1]],
                             shape=args[[2]])

    }

    args <- fetch_gamma_components("ip_to_hosp", standard_api)
    params[["delay_Ip_to_hosp"]] = list(mu=args[[1]],
                             shape=args[[2]])

    args <- fetch_gamma_components("to_icu", standard_api)
    params[["delay_to_icu"]] = list(mu=args[[1]],
                                    shape=args[[2]])
    
    args <- fetch_gamma_components("to_non_icu", standard_api)
    params[["delay_to_non_icu"]] = list(mu=args[[1]],
                                        shape=args[[2]])
    
    args <- fetch_gamma_components("ip_to_death", standard_api)
    params[["delay_Ip_to_death"]] = list(mu=args[[1]],
                                        shape=args[[2]])

    return(params)
}

#' Get Trigger Configuration
#' 
#' Retrieve the Lockdown trigger settings from API.
#' These include whether to lockdown based on an offset
#' from the peak, or based on the ICU bed usage
#' 
#' @param standard_api Instance of standard API object
unpack_trigger <- function(standard_api)
{
    read_estimate <- standard_api$read_estimate

    return(
        list(
            trigger = ifelse(read_estimate("lockdown/config", "isnational") == 1, "national", "local"),
            duration = read_estimate("lockdown/config", "duration"),
            icu_bed_usage = read_estimate("lockdown/triggers", "icu_bed_usage"),
            intervention_shift = read_estimate("lockdown/triggers", "intervention_shift")
        )
    )
}

#' Generate R0 Values
#' 
#' Generates an array of R0 values of length matching
#' the number of runs requested 'n'. The values are generated
#' using the relevant normal distribution retrieved from the API.
#' 
#' @param standard_api Instance of standard API object
#' @param seed The generation seed
#' @param n Number of values to generate
create_R0s <- function(standard_api, seed, n)
{
    np_rand <- import("numpy")$random

    np_rand$seed(ifelse(seed > 0, seed, as.integer(py_time())))

    read_distribution <- standard_api$read_distribution

    norm <- read_distribution("distributions/R0", "R0")

    return(norm$rvs(as.integer(n)))
}

#' Gathers and assembles all arguments
#' 
#' This function performs all operations to retrieve
#' the required data and parameters from the API
#' then assembling the results into a list that matches
#' the form of a local arguments set
#' 
#' @param standard_api Instance of standard API object
objects <- function(standard_api)
{
    read_array <- standard_api$read_array

    read_estimate <- standard_api$read_estimate
    read_table <- standard_api$read_table

    matrix_data <- unpack_matrices(standard_api)

    params <- list(
            age_var_symptom_rates = data.table(read_table("symptomatic_rates/rates_per_age", "age_varying_symptomatic_rates")),
            health_burden_probabilities = data.table(read_table("health_burden_processes/probabilities", "health_burden_processes")),
            contact_matrices = matrix_data$matrices,
            group_names = matrix_data$group_names,
            lockdown_rates = read_table("lockdown/lockdown_rates", "lockdown_rates") %>% as.integer,
            school_holiday_rates = read_table("school/holiday_rates", "school_holiday_rates") %>% as.integer,
            size = unpack_populations(standard_api),
            school_terms = unpack_terms(standard_api),
            seed = unpack_seeding(standard_api),
            fIs = read_estimate("fixed-parameters/relative_infectiousness", "rel_symptomatic"),
            fIp = read_estimate("fixed-parameters/relative_infectiousness", "rel_preclinical"),
            fIa = read_estimate("fixed-parameters/relative_infectiousness", "rel_subclinical"),
            time = unpack_times(standard_api),
            lockdown_trigger = unpack_trigger(standard_api),
            tau = read_estimate("fixed-parameters/tau", "tau"),
            rho = read_estimate("fixed-parameters/rho", "rho")
        )
    params = append(params, unpack_dis_params(standard_api))
    params$ngroups = params$contact_matrices$region$other %>% length %>% sqrt(.)
    params$intervention = unpack_intervention(standard_api, params$ngroups)
    return(params)
}

#' Fetch the bin for a given lower elderly age limit
#' 
#' Retrieves the bin associated with the age given which
#' is the choice of lower boundary above which someone is
#' classed as being elderly
#' 
#' @param age Lower age boundary
#' @param names List of bin labels (0-4, 0 to 4, etc.)
get_elderly_bin_for_age <- function(age, names)
{
    x <- NULL

    if(any(names %>% str_detect(., "-")))
    {
        x <- names %>% head(., -1) %>% strsplit(., "-") %>% lapply(., as.numeric)
    }

    if(any(names %>% str_detect(., "to")))
    {
        x <- names %>% head(., -1) %>% strsplit(., "to") %>% lapply(., as.numeric)
    }

    for(i in 1:length(x))
    {
        if(age >= x[[i]][[1]] && age < x[[i]][[2]])
        {
            return(i)
        }
    }

    return(-1)

}

#' Final argument form
#' 
#' This function is called by the main model run to
#' assemble all arguments from the API and retrieve them
#' 
#' @param standard_api Instance of standard API object
#' @param n_runs number of model runs this session
remote_data <- function(standard_api, n_runs)
{
    # Assume same binning always provided to model
    group_names <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
    "65-69", "70-74", "75+")

    options_print_str <- ""

    # Define fixed parameter choices (those that do not need to be set by the API itself)
    config_params <- list(
        child_grandparent_contacts = FALSE, # This is analysis specific so should be switched off
                                            # to ensure flexibility
        fast_multinomial = FALSE,
        deterministic = FALSE,              # Seed the age distributions (set to False in "vanilla" run UK.R)
        run_mode = "LSHTMModel",            # Normal analysis run as opposed "R0 Analysis" which checks
                                            # intervention effect on R0 (might be needed later as a separate run?)
        elderly_from_bin = 15,              # Bin which defines point at which individual is classed as elderly (set to '65-70')
        dH = 1, dC = 1,                     # Unused by model at the moment
        report_frequency = 4,               # Report every 4 steps
        group_names = group_names           # Assume group names constant
    )

    config_params <- append(config_params, objects(standard_api))

    # Define an elderly individual as 70+
    config_params$elderly_from_bin <- get_elderly_bin_for_age(70, group_names)

    # Randomly generate some R0 values
    config_params$R0s <- create_R0s(standard_api, config_params$seed$value, n_runs)

    return(list(params=config_params, output_str=options_print_str))
}