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

if(is.na(region))
{
  stop("Invalid argument for UK region, should be a string such as 'Glasgow City' etc.")
}

populations <- SCRCdataAPI::read_table(path="SCRC/pipeline_data/population_sizes", 
                          file="0.20200729.0.h5",
                          component="population_size/persons")

populations %<>% data.table

cat(paste("Using Region:", populations[Name %like% region]$`Name`[[1]], "\n"))

survey_obj <- get_survey(file.path(source_root, data_addr))

description <- paste(survey_obj$reference$title, paste0(survey_obj$reference$author[[1]], " et. al."), survey_obj$reference$year, sep=", ")

tmp <- as.Date(date_accessed, format = "%Y-%m-%d")

version_number <- paste("0", gsub("-", "", tmp), "0" , sep = ".")

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

create_array(filename=paste(version_number,"h5",sep="."), component=file.path("contact_matrices", "home"),
             array=ruk_home, path="SCRC/pipeline_data/subregion_contact_matrices",
             dimension_names=list(colvalue=col_names, rowvalue=col_names))
create_array(filename=paste(version_number,"h5",sep="."), component=file.path("contact_matrices", "school"),
             array=ruk_scho, path="SCRC/pipeline_data/subregion_contact_matrices",
             dimension_names=list(colvalue=col_names, rowvalue=col_names))
create_array(filename=paste(version_number,"h5",sep="."), component=file.path("contact_matrices", "work"),
             array=ruk_work, path="SCRC/pipeline_data/subregion_contact_matrices",
             dimension_names=list(colvalue=col_names, rowvalue=col_names))
create_array(filename=paste(version_number,"h5",sep="."), component=file.path("contact_matrices", "other"),
             array=ruk_othe, path="SCRC/pipeline_data/subregion_contact_matrices",
             dimension_names=list(colvalue=col_names, rowvalue=col_names))