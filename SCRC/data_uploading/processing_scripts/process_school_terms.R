library(SCRCdataAPI)
library(RCurl)
library(data.table)
library(tidyverse)
library(magrittr)

author <- "Leeds City Council"
date_accessed <- Sys.Date()
struct_version <- 0
dataset_version <- 0
source_root <- "https://datamillnorth.org"

description <- paste("School terms for the Leeds area published by Leeds City Council,",
                     " for the year 2019")

file_addr <- paste0("download/school-term-times/ea628989-f4e0-4f16-9296-ed2c294993bd/term%2520dates.csv")

key <- read.table("SCRC/data_uploading/token.txt")
product_name <- file.path("schools",
			  "school_terms")

tmp <- as.Date(date_accessed, format = "%Y-%m-%d")

version_number <- paste("0", gsub("-", "", tmp), "0" , sep = ".")

namespace <- "LSHTM"

processed_path <- file.path("data-raw", product_name)

product_storageRoot <- "boydorr"

product_path <- product_name

product_filename <- paste0(version_number, ".h5")

product_storageRootId <- new_storage_root(name = product_storageRoot,
                                          root = "ftp://boydorr.gla.ac.uk/scrc/",
                                          key = key)

namespaceId <- new_namespace(name = namespace,
                             key = key)

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

data <- retrieve_file_data()

create_table(file="data_2.h5", path="SCRC/pipeline_data/school_terms", component="school_terms", df=data)