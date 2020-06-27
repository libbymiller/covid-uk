#######################################################
#               Expected Arguments                    #
#                                                     #
# Single Value Fixed Parameters:                      #
#   run_mode : "R0 Analysis" for the short simulation #
#              on how an intervention affects R0 else #
#              run normal model mode                  #
#   from_bin : bin number which marks the lower age   #
#              bound for someone being defined as     #
#              elderly (default 15 for 16 bin with    #
#              last bin as 75+)                       #
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
#   lockdown_trigger:                                 #
#       trigger : "national"/"local" COVID trigger    #
#       duration : duration of lockdown in days       #
#       icu_bed_usage : occupancy of ICU bed usage    #
#                       to trigger lockdown (option)  #
#       intervention_shift : lockdown trigger based   #
#                            on shift from peak       #
#                                                     #
#    rates: (school term, lockdown)                   #
#           home, work, schools, other, home_elderly  #
#        work_elderly, schools_elderly, other_elderly #
#                                                     #
#    seed:                                            #
#           value, min_age, max_age                   #
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
#   delay_Ip_to_hosp : gamma(mu, shape) delay between #
#                      becoming infectious and being  #
#                      admitted to hospital           #
#   delay_to_icu : gamma(mu, shape) delay between     #
#                  hospital admission and ICU         #
#   delay_to_non_icu : gamma(mu, shape) delay between #
#                      hospital admission and non-ICU #
#   delay_Ip_to_death : gamma(mu, shape) delay between#
#                       infection and death           #
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

build_observer = function(lockdown_trigger) function(time, dynamics) {}

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

build_population_for_region = function(arguments, location)
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

    #n_groups = length(arguments$population$label)
    #FIXME Hardcoded
    n_groups = 16
    #size = arguments$population$count
    group_names = arguments$population$label
    contact_matrices = arguments$contact_matrices[[location]]

    # Organize parameters into form recognised by model
    population_parameter_set = list(
        type = "SEI3R",
        dE = distribution_params$gamma$dE$p,
        dIp = distribution_params$gamma$dIp$p,
        dIa = distribution_params$gamma$dIa$p,
        dIs = distribution_params$gamma$dIs$p,
        dH = fixed_params$dH,
        dC = fixed_params$dC,
        #size = size,
        size = NULL,
        matrices = contact_matrices,
        contact = rep(1, length(contact_matrices)),
        contact_mult = numeric(0),
        contact_lowerto = numeric(0),
        u = rep(adjusted_params$u, n_groups),
        y = rep(adjusted_params$y, n_groups),
        fIp = rep(fixed_params$fIp, n_groups),
        fIs = rep(fixed_params$fIs, n_groups),
        fIa = rep(fixed_params$fIa, n_groups),
        rho = rep(fixed_params$rho, n_groups),
        tau = rep(fixed_params$tau, n_groups),  
        seed_times = 1,
        dist_seed_ages = rep(1, n_groups),
        schedule = list(), # Set time steps for various parameter change events (e.g. scaling of contact matrices)
        observer = NULL,    # Series of callback functions used to trigger events based on variable values
        name = location,
        #group_names = group_names
        group_names = colnames(contact_matrices[[1]])
    )
    
    return(population_parameter_set)
}

build_population_parameters = function(arguments, locations)
{
    #FIXME: Need to remove these hard coded age group bin limits
    #n_groups = length(arguments$population$label)
    n_groups = 16
    all_region_params = list()
    for(region in locations)
    {
        demographics = cm_get_demographics(region, n_groups);
        size = demographics[, round((f + m) * 1000)];
        pars = build_population_for_region(arguments, region)
        pars$size = size
        all_region_params[[region]] = pars
    }

    return(all_region_params)
}

# Get regions for the UK.
cm_uk_locations = function(structure, country, level) {
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
    if (country %like% "E") { locs = c(locs, structure[Code %like% "^E" & Geography1 %in% gE, Name]); }
    if (country %like% "W") { locs = c(locs, structure[Code %like% "^W" & Geography1 %in% gW, Name]); }
    if (country %like% "S") { locs = c(locs, structure[Code %like% "^S" & Geography1 %in% gS, Name]); }
    if (country %like% "N") { locs = c(locs, structure[Code %like% "^N" & Geography1 %in% gN, Name]); }
    return (paste0("UK | ", locs));
}


build_params_from_args = function(arguments)
{
    locations = get_location_labels(arguments)

    population_parameter_sets = build_population_parameters(arguments, locations$location_labels)

    if(dump_params)
    {
        output_file = file.path(covid_uk_path, "output", paste0("mod-initial-params-", gsub(" ", "", gsub(":","",Sys.time())), ".pars"))
        dput(population_parameter_sets, file=output_file)
        message(paste0("Initial Params saved to '", output_file))
    }

    ngroups = length(population_parameter_sets[[1]]$group_names)

    burden_processes = build_burden_processes(ngroups, arguments)

    if(typeof(arguments$fast_multinomial$isTrue) == "logical")
    {
        is_fast_multi = arguments$fast_multinomial$isTrue
    }
    else {
       is_fast_multi = toupper(arguments$fast_multinomial$isTrue) == "TRUE"
    }

    if(typeof(arguments$deterministic$isTrue) == "logical")
    {
        is_deterministic = arguments$deterministic$isTrue
    }
    else {
       is_deterministic = toupper(arguments$deterministic$isTrue) == "TRUE"
    }

    if(typeof(arguments$child_grandparentcontacts$enabled) == "logical")
    {
        child_grandparent_contacts = arguments$child_grandparentcontacts$enabled
    }
    else {
       child_grandparent_contacts = toupper(arguments$child_grandparentcontacts$enabled) == "TRUE"
    }

    parameter_set = list(
        pop = population_parameter_sets,
        date0 = arguments$time$start_date,
        time0 = as.numeric(arguments$time$start),
        time1 = as.numeric(arguments$time$end),
        report_every = as.numeric(arguments$report$frequency),
        fast_multinomial = is_fast_multi,
        deterministic = is_deterministic, 
        travel = diag(length(population_parameter_sets)),
        processes = burden_processes,
        time_step = as.numeric(arguments$time$step)
    )

    # Split off the elderly so their contact matrices can be manipulated separately
    # this doubles the number of matrices in the categories: home, work, school, other
    population_set = cm_split_matrices_ex_in(parameter_set,
                                                       as.numeric(arguments$elderly$from_bin))

    population_parameter_set = build_child_elderly_matrix(population_set, child_grandparent_contacts)

    uk_main_params = build_population_for_region(arguments, locations$uk_label)

    unmodified_set = list(
        pop = list(uk_main_params),
        date0 = arguments$time$start_date,
        time0 = as.numeric(arguments$time$start),
        time1 = as.numeric(arguments$time$end),
        report_every = as.numeric(arguments$report$frequency),
        fast_multinomial = is_fast_multi,
        deterministic = is_deterministic, 
        travel = diag(length(uk_main_params)),
        processes = burden_processes,
        time_step = as.numeric(arguments$time$step)
    )

    # Have to unpack these variables else Rcpp breaks (they are needed globally)
    # Define using the model structure the various region variables
    list2env(list(
        locations = locations$locations,
        london = locations$london,
        england = locations$england,
        wales = locations$wales,
        scotland = locations$scotland,
        nireland = locations$nireland,
        westmid = locations$westmid,
        cumbria  = locations$cumbria),
        env=parent.frame())
    list2env(list(loc_length = length(locations$london)), env=parent.frame())

    # Requires an unmodified version (analog to parametersUK1 in UK.R)

    return(list(unmodified=unmodified_set, parameters=population_parameter_set, uk_population_structure=locations))

}