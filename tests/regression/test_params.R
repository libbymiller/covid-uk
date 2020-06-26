##############################################################################
#                                                                            #
#                          TEST PARAMETER CREATION                           #
#                                                                            #
# This script verifies that the parameters passed into the model are         #
# match those defined prior to any modification (i.e. LSHTM master version)  #
# the script currently compares the first population from both samples       #
#                                                                            #
##############################################################################

library(testit) # For 'assert' function

# Retrieve the base parameters from the testing directory
covid_uk_path = getwd()
base_params = dget(file.path(covid_uk_path, "tests", "test_data",
                            "baseline", "baseline_params_pop1.pars"))

# Get most recently created ".pars" file from output folder
files = list.files(path=file.path(covid_uk_path, "output"),pattern=".pars",
                    full.names = TRUE)
dirs =  dirname(files)
last_file = tapply(files,dirs,function(v) v[which.max(file.mtime(v))])[[1]]

new_params = dget(last_file)

# Check that the column names are identical
assert("Column names do not match", names(base_params) == names(new_params))
message("PASS")

for(col in names(base_params))
{
    message(paste("Testing variable '", col, "' : ", base_params[[col]], "vs", new_params[[col]], "\n"))
    assert(paste("FAILED\n"), base_params[[col]] == new_params[[col]])
    message("PASS\n")
}

message("Success!\n")