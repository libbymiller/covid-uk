##############################################################################
#                                                                            #
#                   HEALTH BURDEN PROCESS PROBABILITIES                      #
#                                                                            #
#   The following script is used to generate a table to represent the        #
#   probabilities for various health burden processes in different age       #
#   groups, e.g. probability of a symptomatic individual becoming            #
#   hospitalised. The original data source has yet to be identified.         #
#   Currently values are read from the included CSV file.                    #
#                                                                            #
#   @author : K. Zarebski (UKAEA)                                            #
#   @date   : last modified 2020-08-12                                       #
#                                                                            #
##############################################################################

library(SCRCdataAPI)
library(data.table)
library(magrittr)

date_accessed <- Sys.Date()
struct_version <- 0
dataset_version <- 0

input_file <- file.path("data", "health_burden_processes.csv")
input_table <- data.table(read.csv(input_file))

token_file <- file.path("SCRC", "data_uploading", "token.txt")
if (!file.exists(token_file)) {
  stop(paste("Failed to find file API token file at", token_file))
}
key <- read.table(token_file)

tmp <- as.Date(date_accessed, format = "%Y-%m-%d")

version_number <- paste(struct_version, gsub("-", "", tmp), dataset_version, sep = ".")
namespace <- "LSHTM"
product_name <- file.path("health_burden_processes", "probabilities")

# where is the data product saved? (locally, before being stored)
product_filename <- paste(version_number, "h5", sep = ".")

create_table(filename = product_filename,
             path = product_name,
             component = "health_burden_processes",
             df = input_table)

# where is the data product stored?
product_storageRoot <- "boydorr"
product_path <- product_name

# data product storage root
product_storageRootId <- new_storage_root(name = product_storageRoot,
                                          root = "ftp://boydorr.gla.ac.uk/scrc/",
                                          key = key)
# namespace
namespaceId <- new_namespace(name = namespace,
                             key = key)


dataProductURIs <- upload_data_product(
  storage_root_id = product_storageRootId,
  name = product_name,
  processed_path = file.path(product_name, product_filename),
  product_path = paste(namespace, product_name, product_filename, sep = "/"),
  version = version_number,
  namespace_id = namespaceId,
  key = key)