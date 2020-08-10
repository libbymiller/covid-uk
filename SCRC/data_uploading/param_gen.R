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

# Start constructing address locations:
#   - LSHTM/fixed-parameters

namespace <- "LSHTM"
prefix <- list(fixed="fixed-parameters", dist="distributions")

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
pb <- progress_bar$new(total = length(params))
for(param in params)
{
    name <- file.path(prefix$fixed, param$name)
    path <- paste("master", namespace, name, sep = "/")
    filename <- paste0(param$version, ".toml")
    component_name <- gsub("^.*/([^/]*)$", "\\1", name)

    create_estimate(filename = filename,
        path = file.path("data-raw", path),
        parameters = as.list(setNames(param$value, component_name)))

    upload_data_product(storage_root_id = storage_rootId,
                    name = name,
                   component_name = component_name,
                   processed_path = file.path("data-raw", path, filename),
                    product_path = file.path(path, filename),
                   version = param$version,
                    namespace_id = namespaceId,
                    key = key)
    pb$tick()
}

# Iterate through prior distibutions creating TOML objects and adding them
# as statements within the DataRegistry
pb <- progress_bar$new(total = length(distributions))
for(dis in distributions)
{
    name <- file.path(prefix$dist, dis$name)
    path <- paste("master", namespace, name, sep = "/")
    filename <- paste0(dis$version, ".toml")
    component_name <- gsub("^.*/([^/]*)$", "\\1", name)

    create_distribution(
        filename = filename,
        file.path("data-raw", path),
        name = dis$name,
        distribution = dis$type,
        parameters = dis$params
    )
    
    upload_data_product(storage_root_id = storage_rootId,
                    name = name,
                    component_name = component_name,
                    processed_path = file.path("data-raw", path, filename),
                    product_path = file.path(path, filename),
                    version = dis$version,
                    namespace_id = namespaceId,
                    key = key)
    pb$tick()
}