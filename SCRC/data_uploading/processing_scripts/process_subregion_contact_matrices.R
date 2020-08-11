##############################################################################
#                                                                            #
#                       REGION/SUBSET CONTACT MATRICES                       #
#                                                                            #
#   The following script is used to obtain the subset contact matrices       #
#   (i.e. those to be modelled - subregion, health board etc)                #
#   these are used in the construction of the model parameter set (labelled  #
#   "sample"/"subregion" set within code) used during a full model run.      #
#                                                                            #
#   @author : K. Zarebski                                                    #
#   @date   : last modified 2020-08-11                                       #
#                                                                            #
##############################################################################

library(socialmixr)
library(SCRCdataAPI)
library(data.table)
library(tidyverse)
library(magrittr)

source_root <- "https://doi.org"
data_addr <- "10.5281/zenodo.1043437"

date_accessed <- Sys.Date()
struct_version <- 0
dataset_version <- 0

# Load requested settings from command line
argv = commandArgs(trailingOnly = TRUE);
argc = length(argv);

region = as.character(argv[1]);

token_file <- file.path("SCRC", "data_uploading", "token.txt")
if(!file.exists(token_file))
{
    stop(paste("Failed to find file API token file at", token_file))
}

key <- read.table("SCRC/data_uploading/token.txt")

if(is.na(region))
{
  stop("Invalid argument for UK region, should be a string such as 'Glasgow City' etc.")
}

populations <- SCRCdataAPI::read_table(path="SCRC/pipeline_data/population/population_sizes/0.20200730.0", 
                          file="0.20200730.0.h5",
                          component="population_size/persons")

populations %<>% data.table

cat(paste("Using Region:", populations[Name %like% region]$`Name`[[1]], "\n"))

survey_obj <- get_survey(file.path(source_root, data_addr))

description <- paste(survey_obj$reference$title, paste0(survey_obj$reference$author[[1]], " et. al."), survey_obj$reference$year, sep=", ")

tmp <- as.Date(date_accessed, format = "%Y-%m-%d")

version_number <- paste("0", gsub("-", "", tmp), "0" , sep = ".")
namespace <- "LSHTM"
product_name <- file.path("contact_matrices", "subregion")

cat(paste("Data Source:", description, "\n"))

ruk_pop <- populations[Name %like% region] %>% subset(., select=-c(Name)) %>% transpose
col_names <- colnames(populations)[2:ncol(populations)]
ruk_pop <- as.vector(ruk_pop[[1]], mode="numeric")

ruk_survey_pop = data.frame(lower.age.limit = seq(0, 75, by = 5),
    population = ruk_pop)

ruk_home = contact_matrix(survey_obj, countries = "United Kingdom", age.limits = seq(0, 75, by = 5), 
    survey.pop = ruk_survey_pop, filter = list(cnt_home = 1), symmetric = T)$matrix
ruk_work = contact_matrix(survey_obj, countries = "United Kingdom", age.limits = seq(0, 75, by = 5), 
    survey.pop = ruk_survey_pop, filter = list(cnt_work = 1), symmetric = T)$matrix
ruk_scho = contact_matrix(survey_obj, countries = "United Kingdom", age.limits = seq(0, 75, by = 5), 
    survey.pop = ruk_survey_pop, filter = list(cnt_school = 1), symmetric = T)$matrix
ruk_othe = contact_matrix(survey_obj, countries = "United Kingdom", age.limits = seq(0, 75, by = 5), 
    survey.pop = ruk_survey_pop, filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0), symmetric = T)$matrix

rownames(ruk_home) <- col_names
colnames(ruk_home) <- col_names
rownames(ruk_scho) <- col_names
colnames(ruk_scho) <- col_names
rownames(ruk_othe) <- col_names
colnames(ruk_othe) <- col_names
rownames(ruk_work) <- col_names
colnames(ruk_work) <- col_names

# where is the data product saved? (locally, before being stored)
product_filename <- paste(version_number,"h5",sep=".")

create_array(filename=product_filename, component=file.path("contact_matrices", "home"),
             array=ruk_home, path=product_name,
             dimension_names=list(colvalue=col_names, rowvalue=col_names))
create_array(filename=product_filename, component=file.path("contact_matrices", "school"),
             array=ruk_scho, path=product_name,
             dimension_names=list(colvalue=col_names, rowvalue=col_names))
create_array(filename=product_filename, component=file.path("contact_matrices", "work"),
             array=ruk_work, path=product_name,
             dimension_names=list(colvalue=col_names, rowvalue=col_names))
create_array(filename=product_filename, component=file.path("contact_matrices", "other"),
             array=ruk_othe, path=product_name,
             dimension_names=list(colvalue=col_names, rowvalue=col_names))

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