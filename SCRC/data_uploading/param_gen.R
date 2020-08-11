##############################################################################
#                                                                            #
#                        LSHTM Model Parameter Upload                        #
#                                                                            #
#   This script adds new entries into the data registry for the LSHTM model  #
#   parameters and creates TOML files which should then be uploaded to the   #
#   relevant storage platform (ScottishCovidResponse/DataRepository on       #
#   GitHub at time of writing).                                              #
#                                                                            #
#   You will require an API token from the registry stored in:               #
    token_file <- file.path(getwd(), "SCRC", "data_uploading", "token.txt")  #
#                                                                            #
#   This script is based on the upload scripts by Sonia Mitchell.            #
#                                                                            #
#   @author K. Zarebski                                                      #
#   @date   last modified 2020-08-10                                         #
    global_version <- "0.1.0"                                                #
#                                                                            #
##############################################################################

library(SCRCdataAPI)    # Requires the latest SCRCdataAPI library
library(progress)
library(magrittr)

# Start constructing address locations:
#   - LSHTM/fixed-parameters

namespace <- "LSHTM"
prefix <- list(fixed="fixed-parameters",
               dist="distributions",
               run_config="run-configuration",
               lockdown="lockdown"
            )

key <- read.table(token_file)

# Assemble remote storage location objects
data_github_url <- "git@github.com:ScottishCovidResponse/DataRepository.git"
productStorageRoot <- "DataRepository"
storage_rootId <- new_storage_root(
  name = productStorageRoot,
  root = "https://raw.githubusercontent.com/ScottishCovidResponse/DataRepository/",
  key = key)

namespaceId <- new_namespace(name = namespace,
                             key = key)

# List of fixed parameters and their values, if a single parameter is updated
# without changing the others the version number should also be set
params <- list(
    list(name="tau",       value=1,      version=global_version),
    list(name="rho",       value=1,      version=global_version)
)

# Parameter set object for TOML files containing more than one parameter each

parameter_set <- function(prefix, set_name, param_names, values, version=global_version)
{
    return(
        list(
            prefix=prefix,
            set_name = set_name,
            param_names=param_names,
            values = values,
            version = version
        )
    )
}

param_sets <- list(
    parameter_set(prefix$run_config,
        "time",
        c("start_day", "end_day", "start_date_posix"),
        c(0, 365, 18290)
    ),
    parameter_set(prefix$run_config,
        "seeding",
        c("seed", "min_age", "max_age", "seeding_min_start_day", "seeding_max_start_day"),
        c(-1, 25, 50, 0, 20)
    ),
    parameter_set(prefix$fixed,
        "relative_infectiousness",
        c("rel_preclinical", "rel_symptomatic", "rel_subclinical"),
        c(1, 1, 0.5)
    ),
    parameter_set(prefix$fixed,
        "delay_gamma",
        c("max_delay_gamma", "time_step_delay_gamma"),
        c(60, 0.25)
    ),
    parameter_set(prefix$lockdown,
        "triggers",
        c("intervention_shift", "icu_bed_usage"),
        c(0, -1)
    ),
    parameter_set(prefix$lockdown,
        "config",
        c("isnational", "duration"),
        c(1, 84)
    )
)

# List of prior distributions and their parameters, if a single distribution is
# updated without changing the others the version number should also be set
distributions <- list(
    list(name="R0", type="normal", params=list(mu=2.675739, sigma=0.5719293), version=global_version),
    list(name="dE", type="gamma", params=list(k=4.0, theta=4.0), version=global_version),
    list(name="dIa", type="gamma", params=list(k=5.0, theta=4.0), version=global_version),
    list(name="dIp", type="gamma", params=list(k=1.5, theta=4.0), version=global_version),
    list(name="dIs", type="gamma", params=list(k=3.5, theta=4.0), version=global_version),
    list(name="to_icu", type="gamma", params=list(k=10, theta=10), version=global_version),
    list(name="to_non_icu", type="gamma", params=list(k=8, theta=8), version=global_version),
    list(name="ip_to_death", type="gamma", params=list(k=22, theta=22), version=global_version),
    list(name="ip_to_hosp", type="gamma", params=list(k=7, theta=7), version=global_version)
)

# Iterate through fixed parameters creating TOML objects and adding them
# as statements within the DataRegistry
# pb <- progress_bar$new(total = length(params))
# for(param in params)
# {
#     name <- file.path(prefix$fixed, param$name)
#     path <- paste("master", namespace, name, sep = "/")
#     filename <- paste0(param$version, ".toml")
#     component_name <- gsub("^.*/([^/]*)$", "\\1", name)

#     create_estimate(filename = filename,
#         path = file.path("data-raw", path),
#         parameters = as.list(setNames(param$value, component_name)))

#     upload_data_product(storage_root_id = storage_rootId,
#                     name = name,
#                    component_name = component_name,
#                    processed_path = file.path("data-raw", path, filename),
#                     product_path = file.path(path, filename),
#                    version = param$version,
#                     namespace_id = namespaceId,
#                     key = key)
#     pb$tick()
# }

# Iterate through parameter sets creating TOML files with multiple items
# and adding them as statements within the DataRegistry
pb <- progress_bar$new(total = length(param_sets))
for(set in param_sets)
{
    name <- file.path(set$prefix, set$set_name)
    path <- paste("master", namespace, name, sep = "/")
    filename <- paste0(set$version, ".toml")
    component_name <- set$set_name %>% gsub("^.*/([^/]*)$", "\\1", .)
    args <- mapply(setNames, set$values, set$param_names)
    create_estimate(filename = filename,
        path = file.path("data-raw", path),
        parameters = as.list(args)
    )
    upload_data_product(storage_root_id = storage_rootId,
                    name = name,
                   component_name = component_name,
                   processed_path = file.path("data-raw", path, filename),
                    product_path = file.path(path, filename),
                   version = set$version,
                    namespace_id = namespaceId,
                    key = key)
    pb$tick()
}

# Iterate through distibutions creating TOML objects and adding them
# as statements within the DataRegistry
# pb <- progress_bar$new(total = length(distributions))
# for(dis in distributions)
# {
#     name <- file.path(prefix$dist, dis$name)
#     path <- paste("master", namespace, name, sep = "/")
#     filename <- paste0(dis$version, ".toml")
#     component_name <- gsub("^.*/([^/]*)$", "\\1", name)

#     create_distribution(
#         filename = filename,
#         file.path("data-raw", path),
#         name = dis$name,
#         distribution = dis$type,
#         parameters = dis$params
#     )
    
#     upload_data_product(storage_root_id = storage_rootId,
#                     name = name,
#                     component_name = component_name,
#                     processed_path = file.path("data-raw", path, filename),
#                     product_path = file.path(path, filename),
#                     version = dis$version,
#                     namespace_id = namespaceId,
#                     key = key)
#     pb$tick()
# }