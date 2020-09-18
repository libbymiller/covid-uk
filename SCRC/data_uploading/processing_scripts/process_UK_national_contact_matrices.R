##############################################################################
#                                                                            #
#                       UK NATIONAL CONTACT MATRICES                         #
#                                                                            #
#   The following script is used to obtain the UK national contact matrices  #
#   these are used in the construction of the UK parameter set (labelled     #
#   "region" set within code) which is used to calculate the R0 adjustment   #
#   only. This is NOT the data set which is being simulated                  #
#   (labelled "sample"/"subregion"). Technically the region parameter        #
#   set/matrices etc could be held constant for every model run, replace     #
#   these at your own risk.                                                  #
#                                                                            #
#   @author : K. Zarebski (UKAEA)                                            #
#   @date   : last modified 2020-08-11                                       #
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
namespace <- "LSHTM"
product_name <- file.path("contact_matrices", "national")

token_file <- file.path("SCRC", "data_uploading", "token.txt")
if(!file.exists(token_file))
{
    stop(paste("Failed to find file API token file at", token_file))
}
key <- read.table(token_file)

# where is the data product saved? (locally, before being stored)
product_filename <- paste(version_number,"h5",sep=".")

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
    create_array(file=product_filename,
                 path=product_name,
                 array=uk_matrices[[matrix]], component=file.path("contact_matrices", matrix), 
                 dimension_names=list(rowvalue=rownames(uk_matrices[[matrix]]), colvalue=colnames(uk_matrices[[matrix]])))
}

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