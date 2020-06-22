'''
cm_base_pop_SEI3R = function(n_groups)
{
    list(
        dE  = cm_delay_gamma(4.0, 4.0, t_max = 60, t_step = 0.25)$p, # Derived from Backer et al Eurosurveillance
        dIp = cm_delay_gamma(2.4, 4.0, t_max = 60, t_step = 0.25)$p, # Derived from Backer et al Eurosurveillance
        dIa = cm_delay_gamma(7.0, 4.0, t_max = 60, t_step = 0.25)$p, # Assumed 7 days subclinical shedding
        dIs = cm_delay_gamma(3.2, 3.7, t_max = 60, t_step = 0.25)$p, # Zhang et al 2020
        dH = 1, # hospitalization ignored
        dC = 1, # no case reporting delay
        
        size = rep(1000, n_groups),
        matrices = list(base = diag(n_groups) * 0.5 + 0.5/n_groups),
        contact = 1,
        contact_mult = numeric(),
        contact_lowerto = numeric(),
        u = rep(0.08, n_groups),
        y = rep(0.5, n_groups),
        fIp = rep(1, n_groups),
        fIs = rep(1, n_groups),
        fIa = rep(0.5, n_groups),
        rho = rep(1, n_groups),
        tau = rep(1, n_groups),
        
        seed_times = 1,
        dist_seed_ages = rep(1, n_groups),
        
        schedule = list(),
        observer = NULL
    )
}

# Get default simulation parameters, SEI3R model
cm_base_parameters_SEI3R = function(n_groups = 1, pop = cm_base_pop_SEI3R(n_groups))
{
    # If just a single population, rather than a list of populations, has been passed to this function, rectify that.
    if (is.character(pop$type)) {
        pop = list(pop);
    }
    
    list(
        time_step = 0.25,
        date0 = "2020-01-01",
        time0 = 0,
        time1 = 365,
        report_every = 4,
        fast_multinomial = F,
        deterministic = T,
        pop = pop,
        travel = diag(length(pop)),
        processes = NULL
    )
}
'''

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
            names(parameters$matrices)[m + b * nmat0] = paste0(names(parameters$matrices)[m + b * n_matrices_initial], b + 1);
            parameters$matrices[[m + (b - 1) * nmat0]] = mask0 * parameters$matrices[[m + (b - 1) * n_matrices_initial]];
            parameters$matrices[[m +       b * nmat0]] = mask1 * parameters$matrices[[m +       b * n_matrices_initial]];
        }
    }
    parameters$contact = rep_len(parameters$contact, n_matrices_initial * (length(bounds) + 1));
    }
    
    return (parameters)
}

#######################################################
#               Expected Arguments                    #
#                                                     #
# Single Value Fixed Parameters:                      #
#   time:                                             #
#       max : Maximum time (in days)                  #
#       step : Time interval (in days)                #
#   rho : ?                                           #
#   tau : ?                                           #
#   fIa : ?                                           #
#   fIs : ?                                           #
#   fIp : ?                                           #
#                                                     #
# Flags:                                              #
#   deterministic : ?                                 #
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

build_params_from_args = function(arguments, settings)
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
    contact_matrix = arguments$contact_matrix
    if(typeof(settings$deterministic) == "logical")
    {
        is_deterministic = settings$deterministic
    }
    else {
       is_deterministic = toupper(settings$deterministic) == "TRUE"
    }
    if(typeof(settings$fast_multinomial) == "logical")
    {
        is_fast_multi = settings$fast_multinomial
    }
    else {
       is_fast_multi = toupper(settings$fast_multinomial) == "TRUE"
    }

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
        contact = rep(1, length(contact_matrix)),
        matrices = contact_matrix,
        group_names = group_names,
        schedule = list(),
        observer = NULL 
    ) 

    parameter_set = list(
        pop = population_parameter_set,
        time0 = as.numeric(arguments$time$start),
        time1 = as.numeric(arguments$time$end),
        report_every = as.numeric(settings$report$frequency),
        fast_multinomial = is_fast_multi,
        deterministic = is_deterministic,
        travel = diag(length(population_parameter_set)),
        process = NULL
    )

    print(parameter_set)

    return(parameter_set)

}