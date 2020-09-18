##############################################################################
#                                                                            #
#                       Build Model Input Structures                         #
#                                                                            #
#   This file aims to replace the default behaviour of the LSHTM. Originally #
#   parameters were constructed for all regions within the UK in sequence    #
#   leading to long run times. Now a single set of parameters is constructed #
#   based on the arguments passed which will ultimately be read from the     #
#   SCRC data pipeline API.                                                  #
#                                                                            #
#   @author : K. Zarebski (UKAEA)                                            #
#   @date   : last modified 2020-08-03                                       #
#                                                                            #
##############################################################################
#                                                                            #
#                            Expected Arguments                              #
#                                                                            #
#   The argument set provided during parameter construction is expected to   #
#   contain the following:                                                   #
#                                                                            #
# Single Value Fixed Parameters:                                             #
#   run_mode : - "R0 Analysis" for the short simulation on how               #
#                intervention affects R0                                     #
#              - Else for any other run normal model (also sets output       #
#                filename label)                                             #
#                                                                            #
#   elderly_from_bin : bin number which marks the lower age bound for        #
#                      someone to be classed as elderly                      #
#                      (default 15 for 16 bin of 5 yrs, last bin "75+")      #                                          #
#   time :                                                                   #
#       - max : Maximum time (in days)                                       #
#       - step : Time interval (in days)                                     #
#       - start: start day                                                   #
#       - end: end day                                                       #
#   rho : ?                                                                  #
#   tau : ?                                                                  #
#   fIa : Asymptomatic infectiousness factor                                 #
#   fIs : Symptomatic infectiousness factor                                  #
#   fIp : Preclinical infectiousness factor                                  #
#   lockdown_trigger:                                                        #
#       - trigger : "national"/"local" COVID trigger                         #
#       - duration : duration of lockdown in days                            #
#       - icu_bed_usage : occupancy of ICU bed usage                         #
#                       to trigger lockdown (option)                         #
#       - intervention_shift : lockdown trigger based on shift from peak max #
#    seed:                                                                   #
#       - value : actual numeric seed                                        #
#       - min_age :                                                          #
#       - max_age :                                                          #
#                                                                            #
# Flags:                                                                     #
#   deterministic : Determines age seeding                                   #
#           (False: use seeding - age distribution)                          #
#   child_elderly_matrix : Construct terms for contact between <15 and 55+   #
#                                                                            #
# Compartment Distribution Parameters:                                       #
#      - dE  : gamma(mu, shape)                                              #
#      - dIp : gamma(mu, shape)                                              #
#      - dIs : gamma(mu, shape)                                              #
#      - dIa : gamma(mu, shape)                                              #
#      - delay_Ip_to_hosp : gamma(mu, shape) delay between becoming          # 
#                           infectious and being admitted to hospital        #
#      - delay_to_icu : gamma(mu, shape) delay between hospital admission    #
#                       and ICU                                              #
#   delay_to_non_icu : gamma(mu, shape) delay between hospital admission     #
#                      and non-ICU                                           #
#   delay_Ip_to_death : gamma(mu, shape) delay between infection and death   #
#                                                                            #
# Array/Table Parameters:                                                    #
#      - population : table of binned population ages for UK regions         #
#      - rates : (school term, lockdown) home, work, schools, other,         #
#                home_elderly, work_elderly, schools_elderly, other_elderly  #
#      - interventions : list(label=list(val, val, ...), ...)                #
#                        matching the above with 9th term being for if       #
#                        the option of child_elderly_matrix is set           #
#                                                                            #
# Matrix Parameters:                                                         #
#      - contact_matrix for given region: list(home, work, school, other)    #
#                                                                            #
##############################################################################

#' Build the Child-Elderly Contact Matrix
#' 
#' This function constructs the matrix between
#' under 15s and over 55s if the relevant option
#' is set to TRUE, if FALSE a matrix of zeros is constructed
#' 
#' @param population_parameters Parameter set on which to append the matrix
#' @param child_grandparent_contacts Whether to add non-zero matrix terms
#' @examples
#' build_child_elderly_matrix(list(pop=list(..), TRUE))
build_child_elderly_matrix = function(population_parameters, child_grandparent_contacts)
{
    # Create additional matrix for child-elderly contacts
    for (j in seq_along(population_parameters$pop))
    {
    # Recover home/other contact matrix
    mat_ref = population_parameters$pop[[j]]$matrices[[1]] + population_parameters$pop[[j]]$matrices[[4]] + 
        population_parameters$pop[[j]]$matrices[[5]] + population_parameters$pop[[j]]$matrices[[8]];
    
    gran = 5/7; # adjustment for weekdays only.
    N = nrow(mat_ref);
    popsize = population_parameters$pop[[j]]$size;
    mat = matrix(0, ncol = N, nrow = N);

    # Add child-grandparent contacts: under 15s to 55+s
    if(child_grandparent_contacts == TRUE)
    {
        cat("[Child-Elderly Contacts]:\n\tAdding Child (<15) - Grandparent (55+) Contacts Matrix\n")
        for (a in 1:3) {
            dist = c(rep(0, 10 + a), mat_ref[a, (11 + a):N]);
            dist = dist/sum(dist);
            mat[a, ] = mat[a, ] + gran * dist;
            mat[, a] = mat[, a] + (gran * dist) * (popsize[a] / popsize);
        }
    }
    # Add child-grandparent contact matrix to population
    population_parameters$pop[[j]]$matrices$gran = mat;
    population_parameters$pop[[j]]$contact = c(population_parameters$pop[[j]]$contact, 0);
    }

    return(population_parameters)
}

# FIXME there are a lot of hard coded numbers here! What are they?
reformat = function(P)
{
  # 70-74,3388.488  75-79,2442.147  80-84,1736.567  85-89,1077.555  90-94,490.577  95-99,130.083  100+,15.834
  x = c(P[1:7], weighted.mean(c(P[8], P[9]), c(3388.488 + 2442.147, 1736.567 + 1077.555 + 490.577 + 130.083 + 15.834)));
  return (rep(x, each = 2))
}

#' Construct the process probability arrays
#' 
#' Function constructs arrays of probabilities for
#' the various stages of ICU and non-ICU cases and deaths
#' using the given arguments set built either via API or local methods
#' 
#' @param arguments Argument set containing the required probabilities
#' 
#' @examples
#' build_burden_processes(list(health_burden_probabilities=list(Prop_symp_hospitalised=0.1,...)))
build_burden_processes = function(arguments)
{
    process_probs = arguments$health_burden_probabilities

    probabilities =  list(
        icu_symptomatic     = reformat(process_probs[, process_probs$Prop_symp_hospitalised*process_probs$Prop_hospitalised_critical]),
        non_icu_symptomatic = reformat(process_probs[, process_probs$Prop_symp_hospitalised * (1 - process_probs$Prop_hospitalised_critical)]),
        deaths_icu          = reformat(process_probs[, process_probs$Prop_critical_fatal]),
        deaths_non_icu      = reformat(process_probs[, process_probs$Prop_noncritical_fatal])
    )

    hfr = process_probs[, process_probs$Prop_noncritical_fatal/process_probs$Prop_symp_hospitalised]

    gamma_Ip_Hosp_delay = cm_delay_gamma(mu = as.numeric(arguments$delay_Ip_to_hosp$mu), 
                                    shape = as.numeric(arguments$delay_Ip_to_hosp$shape), 
                                    t_max = as.numeric(arguments$time$max),
                                    t_step = as.numeric(arguments$time$step))
    
    gamma_to_icu_delay = cm_delay_gamma(mu = as.numeric(arguments$delay_to_icu$mu),
                                        shape = as.numeric(arguments$delay_to_icu$shape),
                                        t_max = as.numeric(arguments$time$max),
                                        t_step = as.numeric(arguments$time$step))
    
    gamma_to_non_icu_delay = cm_delay_gamma(mu = as.numeric(arguments$delay_to_non_icu$mu),
                                            shape = as.numeric(arguments$delay_to_non_icu$shape),
                                            t_max = as.numeric(arguments$time$max),
                                            t_step = as.numeric(arguments$time$step))

    gamma_Ip_Death_delay = cm_delay_gamma(mu = as.numeric(arguments$delay_Ip_to_death$mu), 
                                          shape = as.numeric(arguments$delay_Ip_to_death$shape), 
                                          t_max = as.numeric(arguments$time$max),
                                          t_step = as.numeric(arguments$time$step))

    delay_skip = cm_delay_skip(as.numeric(arguments$time$max), as.numeric(arguments$time$step))

    burden_processes = list(
        list(source = "Ip", type = "multinomial",
             names = c("to_icu", "to_nonicu", "null"),
             report = c("", "", ""),
             prob = matrix(c(probabilities$icu_symptomatic,
                             probabilities$non_icu_symptomatic,
                             1 - probabilities$icu_symptomatic - probabilities$non_icu_symptomatic), 
                             nrow = 3, 
                             ncol = arguments$ngroups, 
                             byrow = TRUE),
             delays = matrix(c(gamma_Ip_Hosp_delay$p, gamma_Ip_Hosp_delay$p, 
                               delay_skip$p), nrow = 3, byrow = TRUE)),
        
        list(source = "to_icu", type = "multinomial", names = "icu", report = "p",
            prob = matrix(1, nrow = 1, ncol = arguments$ngroups, byrow = TRUE),
            delays = matrix(gamma_to_icu_delay$p, nrow = 1, byrow = TRUE)),
        
        list(source = "to_nonicu", type = "multinomial", names = "nonicu", report = "p",
            prob = matrix(1, nrow = 1, ncol = arguments$ngroups, byrow = TRUE),
            delays = matrix(gamma_to_non_icu_delay$p, nrow = 1, byrow = TRUE)),
        
        list(source = "Ip", type = "multinomial", names = c("death", "null"), report = c("o", ""),
            prob = matrix(c(probabilities$deaths_non_icu, 1 - probabilities$deaths_non_icu), 
                            nrow = 2, ncol = arguments$ngroups, byrow = TRUE),
            delays = matrix(c(gamma_Ip_Death_delay$p, delay_skip$p), nrow = 2, byrow = TRUE))
    )

    return (burden_processes)
}

#' Build the parameter subset
#' 
#' Function constructs parameter subset for a single region
#' using the provided arguments. This is an adaptation of the original
#' model run which constructed this subset for every region in tandem.
#' 
#' @param arguments Argument set from which to construct these parameters
#' @param subset_label Label to identify this subset
build_population_for_subset = function(arguments, subset_label)
{
    # Construct Gamma Distributions using the 
    # cm_delay_gamma function for dE, dIp, dIs, dIa

    fixed_params = list( 
        dH = as.numeric(arguments$dH),  # Currently unused
        dC = as.numeric(arguments$dC),  # Unused?

        fIa = as.numeric(arguments$fIa),
        fIs = as.numeric(arguments$fIs),
        fIp = as.numeric(arguments$fIp),

        rho =  as.numeric(arguments$rho),
        tau = as.numeric(arguments$tau)
    )

    distribution_params = list(
        gamma = list(
            dE = cm_delay_gamma(
                mu=as.numeric(arguments$dE$mu), 
                shape=as.numeric(arguments$dE$shape),
                t_max=as.numeric(arguments$time$max),
                t_step=as.numeric(arguments$time$step)
            ),

            dIp = cm_delay_gamma(
                mu=as.numeric(arguments$dIp$mu), 
                shape=as.numeric(arguments$dIp$shape),
                t_max=as.numeric(arguments$time$max),
                t_step=as.numeric(arguments$time$step)
            ),

            dIs = cm_delay_gamma(
                mu=as.numeric(arguments$dIs$mu), 
                shape=as.numeric(arguments$dIs$shape),
                t_max=as.numeric(arguments$time$max),
                t_step=as.numeric(arguments$time$step)
            ),

            dIa = cm_delay_gamma(
                mu=as.numeric(arguments$dIa$mu), 
                shape=as.numeric(arguments$dIa$shape),
                t_max=as.numeric(arguments$time$max),
                t_step=as.numeric(arguments$time$step)
            )
        )
    )

    # Set initial u and y values (value of these does not matter)
    # as result is the same
    u_init = 0.08
    y_init = 0.5

    contact_matrices = arguments$contact_matrices[[subset_label]]
    population_size = arguments$size[[subset_label]]

    # Organize parameters into form recognised by model
    subset_parameter_set = list(
        type = "SEI3R",
        dE = distribution_params$gamma$dE$p,
        dIp = distribution_params$gamma$dIp$p,
        dIa = distribution_params$gamma$dIa$p,
        dIs = distribution_params$gamma$dIs$p,
        dH = fixed_params$dH,
        dC = fixed_params$dC,
        size = population_size,
        matrices = contact_matrices,
        contact = rep(1, length(contact_matrices)),
        contact_mult = numeric(0),
        contact_lowerto = numeric(0),
        u = rep(u_init, arguments$ngroups),
        y = rep(y_init, arguments$ngroups),
        fIp = rep(fixed_params$fIp, arguments$ngroups),
        fIs = rep(fixed_params$fIs, arguments$ngroups),
        fIa = rep(fixed_params$fIa, arguments$ngroups),
        rho = rep(fixed_params$rho, arguments$ngroups),
        tau = rep(fixed_params$tau, arguments$ngroups),  
        seed_times = 1,
        dist_seed_ages = rep(1, arguments$ngroups),
        schedule = list(), # Set time steps for various parameter change events (e.g. scaling of contact matrices)
        observer = NULL,    # Series of callback functions used to trigger events based on variable values
        name = subset_label,
        group_names = arguments$group_names
    )
    
    return(subset_parameter_set)
}

#' Constructs a parameter set for a given set of arguments
#' 
#' Function receives a complete list of arguments in order
#' to then construct a parameter set including a member for population which
#' is itself a parameter subset.
#' 
#' @param arguments Arguments for which to construct the parameters
build_params_from_args = function(arguments)
{
    subset_parameter_set = build_population_for_subset(arguments, 'subset')

    if(dump_params)
    {
        output_file = file.path(covid_uk_path, "output", paste0("Sample-stage1-params-", gsub(" ", "", gsub(":","",Sys.time())), ".pars"))
        dput(subset_parameter_set, file=output_file)
        message(paste0("Initial Params saved to '", output_file))
    }


    burden_processes = build_burden_processes(arguments)

    parameter_set = list(
        pop = list(subset_parameter_set),
        date0 = arguments$time$start_date,
        time0 = as.numeric(arguments$time$start),
        time1 = as.numeric(arguments$time$end),
        report_every = as.numeric(arguments$report_frequency),
        fast_multinomial = arguments$fast_multinomial,
        deterministic = arguments$deterministic, 
        travel = diag(length(list(subset_parameter_set))),
        processes = burden_processes,
        time_step = as.numeric(arguments$time$step)
    )

    # Split off the elderly so their contact matrices can be manipulated separately
    # this doubles the number of matrices in the categories: home, work, school, other
    population_set = cm_split_matrices_ex_in(parameter_set,
                                                       as.numeric(arguments$elderly_from_bin))

    subset_parameter_set = build_child_elderly_matrix(population_set, arguments$child_grandparent_contacts)

    region_params = build_population_for_subset(arguments, 'region')

    if(dump_params)
    {
        output_file = file.path(covid_uk_path, "output", paste0("Region-params-", gsub(" ", "", gsub(":","",Sys.time())), ".pars"))
        dput(region_params, file=output_file)
        message(paste0("Initial Params saved to '", output_file))
    }

    unmodified_set = list(
        pop = list(region_params),
        date0 = arguments$time$start_date,
        time0 = as.numeric(arguments$time$start),
        time1 = as.numeric(arguments$time$end),
        report_every = as.numeric(arguments$report_frequency),
        fast_multinomial = arguments$fast_multinomial,
        deterministic = arguments$deterministic, 
        travel = diag(length(list(region_params))),
        processes = burden_processes,
        time_step = as.numeric(arguments$time$step)
    )

    # Requires an unmodified version (analog to parametersUK1 in UK.R)

    return(list(unmodified=unmodified_set, parameters=subset_parameter_set))

}