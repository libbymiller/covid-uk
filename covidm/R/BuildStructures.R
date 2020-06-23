#######################################################
#               Expected Arguments                    #
#                                                     #
# Single Value Fixed Parameters:                      #
#   time:                                             #
#       max : Maximum time (in days)                  #
#       step : Time interval (in days)                #
#       start: start day                              #
#       end: end day                                  #
#   rho : ?                                           #
#   tau : ?                                           #
#   fIa : ?                                           #
#   fIs : ?                                           #
#   fIp : ?                                           #
#   from_bin : bin at which we define the             #
#              start of 'elderly' age state           #
#                                                     #
# Flags:                                              #
#   deterministic : Determines age seeding            #
#           (False: use seeding - age distribution)   #
#                                                     #
# Adjusted Parameters (modified during run):          #
#   u_init : ?                                        #
#   y_init : ?                                        #
#                                                     #
# Distribution Parameters:                            #
#   dE : gamma(mu, shape)                             #
#   dIp : gamma(mu, shape)                            #
#   dIs : gamma(mu, shape)                            #
#   dIa : gamma(mu, shape)                            #
#                                                     #
# Array Parameters:                                   #
#   population : binned population data               #
#                list(label=list(), count=list()) ?   #
#   interventions :                                   #
#       list(label=list(val, val, ...), ...)          #
#       (seems to be of length 9, no idea why yet)    #
#                                                     #
# Matrix Parameters:                                  #
#   contact_matrix for given region:                  #
#       list(home, work, school, other)               #
#                                                     #
#                                                     #
# Age binning will be inferred from the data being    #
# passed into the model?                              #
#                                                     #
# Will it be safe to assume all arrays passed into    #
# this model will have the same age-binning?          #
#######################################################


# Modified cm_split_matrices_ex_in to be for one population parameter set only

cm_split_matrices_ex_in = function(ngroups, parameters, bounds)
{

    if(any(bounds < 1) | bounds > ngroups)
    {
        stop("Bounds must lie within [1, nrow(mat)] for splitting contact matrices.");
    }

    n_matrices_initial = length(parameters$matrices)
    parameters$matrices = rep(parameters$matrices, length(bounds)+1)

    for (b in seq_along(bounds))
    {
        lb = floor(bounds[b]);
        fb = bounds[b] %% 1;
        mask1 = matrix(1, nrow = ngroups, ncol = ngroups);
        if (lb > 1) {
            mask1[1:(lb - 1), 1:(lb - 1)] = 0;
        }
        mask1[lb, 1:lb] = 1 - fb;
        mask1[1:lb, lb] = 1 - fb;
        mask0 = 1 - mask1;
        
        for (m in seq_len(n_matrices_initial)) {
            names(parameters$matrices)[m + b * n_matrices_initial] = paste0(names(parameters$matrices)[m + b * n_matrices_initial], b + 1);
            parameters$matrices[[m + (b - 1) * n_matrices_initial]] = mask0 * parameters$matrices[[m + (b - 1) * n_matrices_initial]];
            parameters$matrices[[m +       b * n_matrices_initial]] = mask1 * parameters$matrices[[m +       b * n_matrices_initial]];
        }
    }
    parameters$contact = rep_len(parameters$contact, n_matrices_initial * (length(bounds) + 1));
    
    return (parameters)
}

build_observer = function(lockdown_trigger) function(time, dynamics) {}

build_child_elderly_matrix = function(analysis, population_parameters)
{
    # Create additional matrix for child-elderly contacts
    # Recover home/other contact matrix

    # FIXME: This is based on there being 4 additional bins when splitting the matrices
    # need to generalise this and base it on 'from_bin' eventually

    mat_ref = population_parameters$matrices[[1]] + population_parameters$matrices[[4]] + 
        population_parameters$matrices[[5]] + population_parameters$matrices[[8]];
    
    gran = 5/7; # adjustment for weekdays only.
    N = nrow(mat_ref);
    popsize = population_parameters$size;
    mat = matrix(0, ncol = N, nrow = N);
    
    # Add child-grandparent contacts: under 15s to 55+s
    if(analysis == 4)
    {
        for (a in 1:3) {
            dist = c(rep(0, 10 + a), mat_ref[a, (11 + a):N]);
            dist = dist/sum(dist);
            mat[a, ] = mat[a, ] + gran * dist;
            mat[, a] = mat[, a] + (gran * dist) * (popsize[a] / popsize);
        }
    } 
    # Add child-grandparent contact matrix to population
    population_parameters$matrices$gran = mat;
    population_parameters$contact = c(population_parameters$contact, 0);

    return(population_parameters)
}

# FIXME there are a lot of hard coded numbers here! What are they?
# breaks at the moment as 'x' and 'w' are not same length in weighted.mean
reformat = function(P)
{
  # 70-74,3388.488  75-79,2442.147  80-84,1736.567  85-89,1077.555  90-94,490.577  95-99,130.083  100+,15.834
  x = c(P[1:7], weighted.mean(c(P[8], P[9]), c(3388.488 + 2442.147, 1736.567 + 1077.555 + 490.577 + 130.083 + 15.834)));
  return (rep(x, each = 2))
}

build_burden_processes = function(ngroups, arguments)
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
                             ncol = ngroups, 
                             byrow = TRUE),
             delays = matrix(c(gamma_Ip_Hosp_delay$p, gamma_Ip_Hosp_delay$p, 
                               delay_skip$p), nrow = 3, byrow = TRUE)),
        
        list(source = "to_icu", type = "multinomial", names = "icu", report = "p",
            prob = matrix(1, nrow = 1, ncol = ngroups, byrow = TRUE),
            delays = matrix(gamma_to_icu_delay$p, nrow = 1, byrow = TRUE)),
        
        list(source = "to_nonicu", type = "multinomial", names = "nonicu", report = "p",
            prob = matrix(1, nrow = 1, ncol = ngroups, byrow = TRUE),
            delays = matrix(gamma_to_non_icu_delay$p, nrow = 1, byrow = TRUE)),
        
        list(source = "Ip", type = "multinomial", names = c("death", "null"), report = c("o", ""),
            prob = matrix(c(probabilities$deaths_non_icu, 1 - probabilities$deaths_non_icu), 
                            nrow = 2, ncol = ngroups, byrow = TRUE),
            delays = matrix(c(gamma_Ip_Death_delay$p, delay_skip$p), nrow = 2, byrow = TRUE))
    )

    return (burden_processes)
}

build_population_parameters = function(arguments, settings)
{
# Construct Gamma Distributions using the 
    # cm_delay_gamma function for dE, dIp, dIs, dIa

    fixed_params = list( 
        dH = as.numeric(arguments$dH$value),  # Currently unused
        dC = as.numeric(arguments$dC$value),  # Unused?

        fIa = as.numeric(arguments$fIa$value),
        fIs = as.numeric(arguments$fIs$value),
        fIp = as.numeric(arguments$fIp$value),

        rho =  as.numeric(arguments$rho$value),
        tau = as.numeric(arguments$tau$value)
    )

    adjusted_params = list(
        u = as.numeric(arguments$u_init$value),
        y = as.numeric(arguments$y_init$value)
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

    n_groups = length(arguments$population$label)
    size = arguments$population$count
    group_names = arguments$population$label
    contact_matrices = arguments$contact_matrices

    # Organize parameters into form recognised by model
    population_parameter_set = list(
        type = "SEI3R",
        dE = distribution_params$gamma$dE$p,
        dIp = distribution_params$gamma$dIp$p,
        dIa = distribution_params$gamma$Ia$p,
        dIs = distribution_params$gamma$dIs$p,
        dH = fixed_params$dH,
        dC = fixed_params$dC,
        rho = rep(fixed_params$rho, n_groups),
        tau = rep(fixed_params$tau, n_groups),
        u = rep(adjusted_params$u, n_groups),
        y = rep(adjusted_params$y, n_groups),
        fIs = rep(fixed_params$fIs, n_groups),
        fIa = rep(fixed_params$fIa, n_groups),
        fIp = rep(fixed_params$fIp, n_groups),
        size = size,
        contact = rep(1, length(contact_matrices)),
        matrices = contact_matrices,
        group_names = group_names,
        schedule = list(), # Set time steps for various parameter change events (e.g. scaling of contact matrices)
        observer = NULL    # Series of callback functions used to trigger events based on variable values
    )

    return(population_parameter_set)
}

build_params_from_args = function(analysis, arguments, settings)
{
    population_parameter_set = build_population_parameters(arguments, settings)

    ngroups = length(population_parameter_set$group_names)

    # Split off the elderly so their contact matrices can be manipulated separately
    # this doubles the number of matrices in the categories: home, work, school, other
    population_parameter_set = cm_split_matrices_ex_in(ngroups, population_parameter_set,
                                                       as.numeric(arguments$elderly$from_bin))
    
    population_parameter_set = build_child_elderly_matrix(analysis, population_parameter_set)

    burden_processes = build_burden_processes(ngroups, arguments)

    if(typeof(settings$fast_multinomial) == "logical")
    {
        is_fast_multi = settings$fast_multinomial
    }
    else {
       is_fast_multi = toupper(settings$fast_multinomial) == "TRUE"
    }

    if(typeof(settings$deterministic) == "logical")
    {
        is_deterministic = settings$deterministic
    }
    else {
       is_deterministic = toupper(settings$deterministic) == "TRUE"
    }

    parameter_set = list(
        pop = population_parameter_set,
        time0 = as.numeric(arguments$time$start),
        time1 = as.numeric(arguments$time$end),
        report_every = as.numeric(settings$report$frequency),
        fast_multinomial = is_fast_multi,
        deterministic = is_deterministic, 
        travel = diag(length(population_parameter_set)),
        processes = burden_processes
    )

    return(parameter_set)

}