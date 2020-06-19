library(qs)
library(readxl)

local_data_files = list(
    contact_matrices = file.path('configuration', 'all_matrices.rds'),
    uk_population = list(address = file.path('data', 'ukmidyearestimates20192020ladcodes.xls'),
                         sheet = "MYE1",
                         range = "A12:B31"
    )
)

local_data = function(covid_dir, location)
{
    cm = readRDS(file.path(covid_dir, local_data_files$contact_matrices))
    cm = cm[[location]]
    cm = list(home=cm[["home"]], work=cm[["work"]], school=cm[["other"]],
            other=cm[["other"]])
    population = read_xls(file.path(covid_dir, local_data_files$uk_population$address), 
                          sheet=local_data_files$uk_population$sheet, range=local_data_files$uk_population$range)

    # Convert age bins to match contact matrices, i.e. 75+ is last bin not 100+
    corr_pop = population[["...2"]][-c(17,18,19)]
    corr_pop[[16]] = population[["...2"]][16]+population[["...2"]][17]+population[["...2"]][18]+population[["...2"]][19]

    return(list(contact_matrix=cm, population=corr_pop, labels=colnames(cm$home)))
}