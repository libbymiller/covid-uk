##############################################################################
#                                                                            #
#                             LOCKDOWN RATES                                 #
#                                                                            #
#   The following script is used to generate an array of lockdown            #
#   rates, these are pre-factors to the 9 matrices defined during the        #
#   model run for work, school, home, other (young/elderly) etc              #
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

##############################################################
#                       RATE VALUES                          #

rates <- list(
    home = 1,
    work = 0.1,
    schools = 0.1,
    other = 0.1,
    home_elderly = 1,
    work_elderly = 0.1,
    schools_elderly = 0.1,
    other_elderly = 0.1,
    child_elderly = 1
)

mat_rates <- data.frame(rates)
rownames(mat_rates) <- c("rate")
colnames(mat_rates) <- names(rates)
##############################################################

token_file <- file.path("SCRC", "data_uploading", "token.txt")
if(!file.exists(token_file))
{
    stop(paste("Failed to find file API token file at", token_file))
}

key <- read.table(token_file)

tmp <- as.Date(date_accessed, format = "%Y-%m-%d")

version_number <- paste(struct_version, gsub("-", "", tmp), dataset_version , sep = ".")
namespace <- "LSHTM"
product_name <- file.path("lockdown", "lockdown_rates")

# where is the data product saved? (locally, before being stored)
product_filename <- paste(version_number,"h5",sep=".")

create_table(filename = product_filename,
             path = product_name,
             component = "lockdown_rates",
             df = mat_rates)

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