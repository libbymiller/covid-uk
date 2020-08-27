argv = commandArgs(trailingOnly = TRUE);
quiet = grep('--quiet', argv, value = FALSE)
quiet = length(quiet) > 0
cpu_search = grep('--ncpus*', argv, value = TRUE)
if(length(cpu_search) > 0)
{
  ncpus = as.integer(strsplit(cpu_search, split = '=')[[1]][[2]]);
} else {
  ncpus = 1
}

options(repos="https://cran.rstudio.com", Ncpus=ncpus )
install.packages("remotes")
library(remotes)
remotes::install_github("traversc/qs@legacy")
install.packages("devtools")
library(devtools)
devtools::install_version("cpp11", version = "0.1")
install.packages("curl", quiet=quiet)
install.packages("httr", quiet=quiet)
install.packages("rvest", quiet=quiet)
install.packages("rlang", quiet=quiet)
install.packages("stringr", quiet=quiet)
install.packages("data.table", quiet=quiet)
install.packages("ggplot2", quiet=quiet)
install.packages("lubridate", quiet=quiet)
install.packages("nloptr", quiet=quiet)
install.packages("HDInterval", quiet=quiet)
install.packages("cowplot", quiet=quiet)
install.packages("testit", quiet=quiet)
install.packages("readxl", quiet=quiet)
install.packages("ini", quiet=quiet)
install.packages("tidyverse", quiet=quiet)
install.packages("magrittr", quiet=quiet)
install.packages("lubridate", quiet=quiet)
install.packages("testit", quiet=quiet)
install.packages("RcppGSL", quiet=quiet)
install.packages("reticulate", quiet=quiet)
install.packages("socialmixr", quiet=quiet)
remotes::install_github("ScottishCovidResponse/SCRCdataAPI")
