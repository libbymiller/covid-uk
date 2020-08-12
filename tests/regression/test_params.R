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

if(!file.exists(file.path(covid_uk_path, "tests", "test_data",
                            "baseline", "params-Regional-stage1.pars")))
{
	stop("Failed to find test data.")
}

base_params1 = dget(file.path(covid_uk_path, "tests", "test_data",
                            "baseline", "params-Regional-stage1.pars"))
base_params2 = dget(file.path(covid_uk_path, "tests", "test_data",
                            "baseline", "params-Regional-stage2.pars"))

# Only check Glasgow City
base_params1 = base_params1[[158]]
base_params2$pop = list(base_params2$pop[[158]])

file_stage1 = Sys.glob(file.path(covid_uk_path, "output", "Sample-stage1*.pars"))
file_stage2 = Sys.glob(file.path(covid_uk_path, "output", "Sample-stage2*.pars"))

file_stage1 = file_stage1[length(file_stage1)]
file_stage2 = file_stage2[length(file_stage2)]

cat(paste("Opening: ", file_stage1, "\n"))
cat(paste("Opening: ", file_stage2, "\n"))

new_params1 = dget(file_stage1)

# Check that the column names are identical
message("STAGE 1: ")
message("Testing Column Names Match: ")
assert(paste("\nColumn names do not match", "\n"),length(setdiff(names(base_params1[[1]]),names(new_params1[[1]]))) == 0)
message("PASS")

for(col in names(base_params1))
{
    # These variables are set later on and so do not need to be compatible
    if(col %in% c("contact_mult", "contact_lowerto", "schedule", "observer", "name"))
    {
        next;
    }

    # Check names of matrices
    if(col == "matrices")
    {
        message(paste("Testing variable:", col, "\n"))
        assert(paste("FAILED\n"), length(setdiff(base_params1[[col]]$names, new_params1[[col]]$names)) == 0)
        message("PASS\n")
        next;
    }
    message(paste("Testing variable:", col, "\n"))
    assert(paste("FAILED\n", "\t1: ", base_params1[[col]], "\n\t2:", new_params1[[col]], "\n"), base_params1[[col]] == new_params1[[col]])
    message("PASS\n")
}

message("Passed Stage 1")
# Check that the column names are identical
message("STAGE 2: ")
new_params2 = dget(file_stage2)
message("Testing Column Names Match: ")
print(names(base_params2))
assert(paste("\nColumn names do not match", "\n"),length(setdiff(names(base_params2),names(new_params2))) == 0)
message("PASS")
for(col in names(base_params2))
{
     # 'travel' will not match as we are only running on one region, not all of them
    if(col %in% c("pop", "time1", "travel"))
    {
        next;
    }
    message(paste("Testing variable: ", col, "\n"))
    assert(paste("FAILED\n", "\t1: ", base_params2[[col]], "\n\t2:", new_params2[[col]], "\n"), length(setdiff(base_params2[[col]], new_params2[[col]])) == 0)
    message("PASS\n")
}

for(col in names(base_params2$pop[[1]]))
{
    # Only check variables that should have changed
    # u and y are altered depending on random number generation so are included in exceptions
    if(col %in% c("contact_mult", "contact_lowerto", "observer", "seed_times", "name", "u", "y"))
    {
        next;
    }

    if(col == "schedule")
    {
        message(paste("Testing variable: ", col, "\n"))
        for(subcol in names(base_params2$pop[[1]][[col]][[1]]))
        {
            message(paste("\tTesting member: ", subcol, "\n"))
            assert(paste("FAILED\n"), base_params2$pop[[1]][[col]][[1]][[subcol]] ==  new_params2$pop[[1]][[col]][[1]][[subcol]])
            message("\tPASS\n")      
        }
        next;
    }
    # Check names of matrices
    if(col == "matrices")
    {
        message(paste("Testing variable: ", col, "\n"))
        assert(paste("FAILED\n"), length(setdiff(base_params2$pop[[1]][[col]]$names, new_params2$pop[[1]][[col]]$names)) == 0)
        message("PASS\n")
        next;
    }
    message(paste("Testing variable:", col, "\n"))
    assert(paste("FAILED\n", "\t1: ", base_params2$pop[[1]][[col]], "\n\t2:", new_params2$pop[[1]][[col]], "\n"), base_params2$pop[[1]][[col]] == new_params2$pop[[1]][[col]])
    message("PASS\n")
}
message("Passed Stage 2")
message("Success!\n")
