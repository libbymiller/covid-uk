library(SCRCdataAPI)
library(RCurl)
library(data.table)
library(tidyverse)

author <- "Office for National Statistics"
date_accessed <- Sys.Date()
struct_version <- 0
dataset_version <- 0
source_root <- "https://www.ons.gov.uk"

description <- paste("Population Estimates for the UK, England and Wales,",
                     "Scotland and Northern Ireland: Mid-2019, using April 2020",
                     "local authority district codes")

file_addr <- paste0("file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2",
                    "fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwales",
                    "scotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes",
                    "/ukmidyearestimates20192020ladcodes.xls")

retrieve_file_data <- function()
{
    download.file(file.path(source_root, file_addr), 
                  (tf1 <- tempfile(fileext = ".xls")), mode = "wb")
    males <- data.table(readxl::read_xls(tf1, sheet="MYE2 - Males", skip = 4, n_max=426))
    females <- data.table(readxl::read_xls(tf1, sheet="MYE2 - Females", skip = 4, n_max=426))
    persons <- data.table(readxl::read_xls(tf1, sheet="MYE2 - Persons", skip = 4, n_max=426))

    cols <- 5:ncol(males) %>% append(2, .)

    return(
        list(
            males = males %>% .[, ..cols],
            females = females %>% .[, ..cols],
            persons = persons %>% .[, ..cols]
        )
    )
    
}

rebin <- function(data)
{
    dt <- data.table(data[,1])
    colnames <- c(colnames(data)[[1]])
    for(i in seq(2, ncol(data)-15, by=5))
    {
        if(i == ncol(data)-15)
        {
            cols <- i:ncol(data)
            colnames <- append(colnames, "75+")
        }
        else
        {
            cols <- i:i+4
            colnames <- append(colnames, paste0(colnames(data)[[i]], "-", colnames(data)[[i+4]]))
        }
        subs <- data[, ..cols]
        dt <- cbind(dt, rowSums(subs))
    }

    colnames(dt) <- colnames
    
    return(dt)
}

make_table <- function(table, component_name, file_name=NA)
{
    file_name <- ifelse(!is.na(file_name), file_name, paste(struct_version, date_accessed, dataset_version, "h5", sep=".")) %>% gsub("-", "", .)
    create_table(filename=file_name, component=component_name, df=table, path=getwd())
}

tables <- retrieve_file_data() %>% map(., rebin) %>% map(., as.data.frame)

for(table in names(tables))
{
    make_table(tables[[table]], file.path("population_size", table))
}