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

base_params1 = dget(file.path(covid_uk_path, "tests", "test_data",
                            "baseline", "baseline-pop-stage1.pars"))
base_params2 = dget(file.path(covid_uk_path, "tests", "test_data",
                            "baseline", "baseline-stage2.pars"))

file_stage1 = Sys.glob(file.path(covid_uk_path, "output", "mod-initial*.pars"))
file_stage2 = Sys.glob(file.path(covid_uk_path, "output", "mod-all*.pars"))

file_stage1 = file_stage1[length(file_stage1)]
file_stage2 = file_stage2[length(file_stage2)]

cat(paste("Opening: ", file_stage1, "\n"))
cat(paste("Opening: ", file_stage2, "\n"))

new_params1 = dget(file_stage1)
new_params2 = dget(file_stage2)

# Check that the column names are identical
message("STAGE 1: ")
message("Testing Column Names Match: ")
assert(paste("\nColumn names do not match", "\n"),length(setdiff(names(base_params1[[1]]),names(new_params1[[1]]))) == 0)
message("PASS")

for(region in 1:length(base_params1))
{
    message(paste("Testing Region: ", base_params1[[region]]$name))
    for(col in names(base_params1[[region]]))
    {
        # These variables are set later on and so do not need to be compatible
        if(col %in% c("contact_mult", "contact_lowerto", "schedule", "observer"))
        {
            next;
        }

        # Check names of matrices
        if(col == "matrices")
        {
            message(paste("Testing variable:", col, "\n"))
            assert(paste("FAILED\n"), length(setdiff(base_params1[[region]][[col]]$names, new_params1[[region]][[col]]$names)) == 0)
            message("PASS\n")
            next;
        }
        message(paste("Testing variable:", col, "\n"))
        assert(paste("FAILED\n", "\t1: ", base_params1[[region]][[col]], "\n\t2:", new_params1[[region]][[col]], "\n"), base_params1[[region]][[col]] == new_params1[[region]][[col]])
        message("PASS\n")
    }
}
message("Passed Stage 1")
# Check that the column names are identical
message("STAGE 2: ")
message("Testing Column Names Match: ")
assert(paste("\nColumn names do not match", "\n"),length(setdiff(names(base_params2),names(new_params2))) == 0)
message("PASS")
for(col in names(base_params2))
{
    if(col %in% c("pop", "time1"))
    {
        next;
    }
    message(paste("Testing variable: ", col, "\n"))
    assert(paste("FAILED\n", "\t1: ", base_params2[[col]], "\n\t2:", new_params2[[col]], "\n"), length(setdiff(base_params2[[col]], new_params2[[col]])) == 0)
    message("PASS\n")
}
for(region in 1:length(base_params2$pop))
{
    message(paste("Testing Region: ", base_params2$pop[[region]]$name))
    for(col in names(base_params2$pop[[region]]))
    {
        # These variables are set later on and so do not need to be compatible
        if(col %in% c("contact_mult", "contact_lowerto", "schedule", "observer"))
        {
            next;
        }

        # Check names of matrices
        if(col == "matrices")
        {
            message(paste("Testing variable: ", col, "\n"))
            assert(paste("FAILED\n"), length(setdiff(base_params2$pop[[region]][[col]]$names, new_params2$pop[[region]][[col]]$names)) == 0)
            message("PASS\n")
            next;
        }
        message(paste("Testing variable:", col, "\n"))
        assert(paste("FAILED\n", "\t1: ", base_params2$pop[[region]][[col]], "\n\t2:", new_params2$pop[[region]][[col]], "\n"), base_params2$pop[[region]][[col]] == new_params2$pop[[region]][[col]])
        message("PASS\n")
    }
}
message("Passed Stage 2")
message("Success!\n")