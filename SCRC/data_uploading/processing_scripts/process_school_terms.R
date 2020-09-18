##############################################################################
#                                                                            #
#                               SCHOOL TERMS                                 #
#                                                                            #
#   The following script is used to generate a table of school terms which   #
#   consists of two columns "school_open", "school_closed"                   #
#   These demo data are currently taken from Leeds City Council.             #
#                                                                            #
#   @author : K. Zarebski (UKAEA)                                            #
#   @date   : last modified 2020-08-11                                       #
#                                                                            #
##############################################################################
library(SCRCdataAPI)
library(RCurl)
library(data.table)
library(tidyverse)
library(magrittr)

#------------------------------- DATA INFO ----------------------------------#

author <- "Leeds City Council"
date_accessed <- Sys.Date()
struct_version <- 0
dataset_version <- 0
source_root <- "https://datamillnorth.org"

description <- paste("School terms for the Leeds area",
                     "published by Leeds City Council,",
                     "for the year 2019")

file_addr <- paste0("download/school-term-times/",
                    "ea628989-f4e0-4f16-9296-ed2c294993bd/term%2520dates.csv")

#-----------------------------------------------------------------------------#

token_file <- file.path("SCRC", "data_uploading", "token.txt")
if(!file.exists(token_file))
{
    stop(paste("Failed to find file API token file at", token_file))
}

key <- read.table(token_file)

tmp <- as.Date(date_accessed, format = "%Y-%m-%d")

version_number <- paste(struct_version, gsub("-", "", tmp), dataset_version, sep = ".")
namespace <- "LSHTM"
product_name <- file.path("school", "school_terms")

# where is the data product saved? (locally, before being stored)
product_filename <- paste(version_number,"h5",sep=".")

retrieve_file_data <- function()
{
    download.file(file.path(source_root, file_addr), (tf1 <- tempfile(fileext = ".csv")))
    data <- data.table(read.csv(tf1))[schoolYear %in% c("2018/19", "2019/20")]
    data$date <- strptime(as.character(data$date), "%d/%m/%Y") %>% format(., "%Y-%m-%d")
    data %<>% select(., date, schoolStatus)
    open <- data[schoolStatus == "Open"][, date]
    close <- data[schoolStatus == "Close"][, date]
    dt <- data.table(open, close)

    return(dt)
}

create_table(filename =product_filename, 
             path = product_name,
             component = "school_terms",
             df = retrieve_file_data())

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