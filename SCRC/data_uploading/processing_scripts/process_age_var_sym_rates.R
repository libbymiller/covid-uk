##############################################################################
#                                                                            #
#                   AGE VARYING SYMPTOMATIC RATES                            #
#                                                                            #
#   The following script is used to generate a table to represent the        #
#   age varying symptomatic rates data, the exact structure of which is      #
#   yet to be fully understood, and the sources still need identifying.      #
#   The data appears to be the result of fitting using set parameters and    #
#   a method not contained within the model code.                            #
#   Currently the values are read from the qs file contained within the      #
#   included model data.                                                     #
#                                                                            #
#   @author : K. Zarebski (UKAEA)                                            #
#   @date   : last modified 2020-08-12                                       #
#                                                                            #
##############################################################################

library(qs)
library(SCRCdataAPI)
library(data.table)
library(magrittr)

date_accessed <- Sys.Date()
struct_version <- 0
dataset_version <- 0

input_file <- file.path("data", "2-linelist_symp_fit_fIa0.5.qs")
input_table <- data.table(qread(input_file))

token_file <- file.path("SCRC", "data_uploading", "token.txt")
if (!file.exists(token_file)) {
  stop(paste("Failed to find file API token file at", token_file))
}
key <- read.table(token_file)

tmp <- as.Date(date_accessed, format = "%Y-%m-%d")

version_number <- paste(struct_version, gsub("-", "", tmp), dataset_version, sep = ".")
namespace <- "LSHTM"
product_name <- file.path("symptomatic_rates", "rates_per_age")

# where is the data product saved? (locally, before being stored)
product_filename <- paste(version_number, "h5", sep = ".")

create_table(filename = product_filename,
             path = product_name,
             component = "age_varying_symptomatic_rates",
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