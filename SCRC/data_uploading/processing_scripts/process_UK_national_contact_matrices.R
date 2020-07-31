##############################################################################
#                                                                            #
#                       UK NATIONAL CONTACT MATRICES                         #
#                                                                            #
#   The following script is used to obtain the UK national contact matrices  #
#   these are used in the construction of the UK parameter set (labelled     #
#   "region" set within code) which is used to calculate the R0 adjustment   #
#   only. This is NOT the data set which is being simulated                  #
#   (labelled "sample"). Technically the region parameter set/matrices etc   #
#   could be held constant for every model run, replace these at your own    #
#   risk.                                                                    #
#                                                                            #
##############################################################################

library(tidyverse)
library(data.table)
library(SCRCdataAPI)

source_root <- "https://www.ncbi.nlm.nih.gov"

description <- paste("Paper: 'Contact Matrices for 152 countries",
                     "Projecting social contact matrices in 152 countries",
                     " using contact surveys and demographic data'",
                     "A. R. Cook et. al., categories: 'home', 'other', 'school', 'work'")

file_addr <- "pmc/articles/PMC5609774/bin/pcbi.1005697.s002.zip"

date_accessed <- Sys.Date()
struct_version <- 0
dataset_version <- 0

tmp <- as.Date(date_accessed, format = "%Y-%m-%d")

version_number <- paste(struct_version, gsub("-", "", tmp), dataset_version , sep = ".")

download.file(file.path(source_root, file_addr), (tf1 <- tempfile(fileext = ".zip")))
unzip(tf1, exdir=(tf2 <- tempdir()))
data_root <- file.path(tf2, "contact_matrices_152_countries")


sources = list(
    matrices = list(
        work = list(file_name=file.path(data_root, "MUestimates_work_2.xlsx"),
                        params=list(sheet = "United Kingdom of Great Britain"),
                        source="Projecting social contact matrices in 152 countries using contact surveys and demographic data"),
        school = list(file_name=file.path(data_root, "MUestimates_school_2.xlsx"),
                            params=list(sheet = "United Kingdom of Great Britain"),
                            source="Projecting social contact matrices in 152 countries using contact surveys and demographic data"),
        other = list(file_name=file.path(data_root, "MUestimates_other_locations_2.xlsx"),
                            params=list(sheet = "United Kingdom of Great Britain"),
                            source="Projecting social contact matrices in 152 countries using contact surveys and demographic data"),
        home = list(file_name=file.path(data_root, "MUestimates_home_2.xlsx"),
                            params=list(sheet = "United Kingdom of Great Britain"),
                            source="Projecting social contact matrices in 152 countries using contact surveys and demographic data")
    )                    
)

fetch_contact_matrix <- function(file_loc, sheet)
{
    # Data source is in bins of 5 years of age
    n_years_per_bin <- 5
    
    matrix <- readxl::read_xlsx(file_loc, sheet=sheet, col_names=FALSE) %>% as.matrix

    max_bin_lower_bound <- n_years_per_bin*(ncol(matrix)-1)

    labels  = c()

    for(i in 0:(ncol(matrix)-1))
    {
        if(i == ncol(matrix)-1)
        {
            labels <- append(labels, c(paste0(i*5, "+")))
        }
        else {
          labels <-append(labels, c(paste0(i*5,"-",(i+1)*5-1)))
        }  
    }

    colnames(matrix) <- labels
    rownames(matrix) <- labels

    return(matrix)
}

fetch_all_matrices_for_UK <- function(data_loc)
{
    matrices = list()

    for(matrix in names(sources$matrices))
    {
        matrices[[matrix]] = fetch_contact_matrix(sources$matrices[[matrix]]$file_name,
                                                sources$matrices[[matrix]]$params$sheet
                                                )
    }
    return(matrices)
}

uk_matrices <- fetch_all_matrices_for_UK()

for(matrix in names(uk_matrices))
{
    create_array(file=paste(version_number,"h5",sep="."),
                 path="SCRC/pipeline_data/uk_national_contact_matrices",
                 array=uk_matrices[[matrix]], component=file.path("uk_national_matrices", matrix), 
                 dimension_names=list(rowvalue=rownames(uk_matrices[[matrix]]), colvalue=colnames(uk_matrices[[matrix]])))
}