##############################################################################
#                                                                            #
#                  COVID-UK MODEL: TEST DEV BRANCH                           #
#                                                                            #
# Regression tests to ensure consistency between versions of the covid-uk    #
# adapted for comparison by the SCRC. This test is designed to be run on a   #
# branch before it is merged into master.                                    #
# Ensure you git pull in master before running this code.                    #
#                                                                            #
# NOTE: Assumes dataframes already exist for 'master' branch in 'test_data'  #
#                                                                            #
# Initially starting with general test which just checks that the output     #
# R dataframes created with the given seeds match with original source       #
# data from running the original code is held within a pickle object file    #
# read in before commencing testing.                                         #
#                                                                            #
# @date       :  last modified 2020-05-07                                    #
# @authors    :  K. Zarebski,                                                #
#                                                                            #
##############################################################################

import argparse         # Specify which branch is being tested
import unittest         # Use Unit Test to automate tests
import logging          # Custom loggers for tests
import pandas           # Read in R dataframes into Pandas DataFrames
import glob             # Read in dataframes from each branch output folder
import os               # File Path Concatenation
import pickle           # Open Pickled DataFrames
from git import Repo    # Use current branch name for file name

from collections import Counter  # Count category entries as part of testing


# Obtain name of current branch to use for labelling
git_branch = Repo(os.getcwd()).active_branch

# Set logging level to INFO
logging.basicConfig(level=logging.INFO)


def getDataFrames(file_addrs):
    '''Retrieve Dataframes into a Dictionary'''
    frames = {}
    for F in file_addrs:
        with open(F, 'rb') as f:
            frames[F.split('/')[-1].split('-')[1]] = pickle.load(f)

    return frames

# Address of files to compare with
baseline_data = glob.glob(os.path.join(os.getcwd(), 'tests', 'test_data', '*baseline.pckl'))

# Check that the files do exist
if not baseline_data:
    print('Unable to find comparison data (should be in the form covid-uk/tests/test_data/<analysis-id>-<category>-baseline.pckl')
    raise FileNotFoundError


# Retrieve DataFrames from original source code run and those obtained from the current scripts
_df_master = getDataFrames(baseline_data)
_df_dev =  getDataFrames(i.replace('baseline', str(git_branch)) for i in baseline_data)

class TestCOVIDUK(unittest.TestCase):
    def testAllDataFramesPresent(self):
        '''Test that the same number of data files have been created'''
        assert all(i in _df_dev for i in _df_master)

    def testConsistentEntriesPerScenario(self):
        '''Test that the number of rows matching each scenario is consistent'''
        for filename in _df_master:
            _count_master = Counter(_df_master[filename]['scenario'].values)
            _count_dev = Counter(_df_dev[filename]['scenario'].values)

            for key in _count_master:
                assert _count_master[key] == _count_dev[key]

    def testConsistentTotals(self):
        '''Check that the values in the 'total' column of the 'totals' file are consistent'''
        assert list(_df_master['totals']['total'].values) == list(_df_dev['totals']['total'].values)

    def testConsistentValues(self):
        '''Check that the values in the 'value' column of the 'dynamics' file are consistent'''
        assert list(_df_master['dynamics']['value'].values) == list(_df_dev['dynamics']['value'].values)
        
if __name__ in "__main__":
    unittest.main()
