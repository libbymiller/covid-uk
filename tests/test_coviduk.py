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
#       Tests should be run using 'nosetests .' from repo root directory     #
#                                                                            #
# Initially starting with general test which just checks that the output     #
# R dataframes created with the given seeds match with original source       #
# data from running the original code is held within a pickle object file    #
# read in before commencing testing.                                         #
#                                                                            #
# @author    :  K. Zarebski (UKAEA)                                          #
# @date      :  last modified 2020-05-11                                    #
#                                                                            #
##############################################################################

import argparse         # Specify which branch is being tested
import unittest         # Use Unit Test to automate tests
import logging          # Custom loggers for tests
import pandas           # Read in R dataframes into Pandas DataFrames
import glob             # Read in dataframes from each branch output folder
import os               # File Path Concatenation
import pickle           # Open Pickled DataFrames

from collections import Counter  # Count category entries as part of testing

# Set logging level to INFO
logging.basicConfig(level=logging.DEBUG)

test_logger = logging.getLogger('TestCOVIDUK')


def getDataFrames(file_addrs):
    '''Retrieve Dataframes into a Dictionary'''
    frames = {'dynamics' : {}, 'totals' : {}}
    for F in file_addrs:
        with open(F, 'rb') as f:
            _key = F.split('/')[-1].split('-')[0]
            if 'dynamics' in F:
                frames['dynamics'][_key] = pickle.load(f)
            else:
                frames['totals'][_key] = pickle.load(f)

    return frames

# Address of files to compare with
baseline_data = glob.glob(os.path.join(os.getcwd(), 'tests', 'test_data', 'baseline', '*baseline.pckl'))

# Check that the files do exist
if not baseline_data:
    print('Unable to find comparison data (should be in the form covid-uk/tests/test_data/baseline/<analysis-id>-<category>-baseline.pckl')
    raise FileNotFoundError


# Retrieve DataFrames from original source code run and those obtained from the current scripts
_df_master = getDataFrames(baseline_data)
_df_dev =  getDataFrames(i.replace('baseline', 'test') for i in baseline_data)

class TestCOVIDUK(unittest.TestCase):
    def testAllDataFramesPresent(self):
        '''Test that the same number of data files have been created'''
        test_logger.info("Checking All Dataframes are present in all files...")
        assert all(i in _df_dev for i in _df_master)

    def testConsistentEntriesPerScenario(self):
        '''Test that the number of rows matching each scenario is consistent'''
        test_logger.info("Checking number of rows per category is consistent with data from source master...")
        for category in _df_master:
            for filename in _df_master[category]:
                if category == 'totals' and filename == '5':
                    continue    # Analysis 5 "R0 Analysis" returns empty 'totals' dataframe
                _count_master = Counter(_df_master[category][filename]['scenario'].values)
                _count_dev = Counter(_df_dev[category][filename]['scenario'].values)

                for key in _count_master:
                    assert _count_master[key] == _count_dev[key]

    def testConsistentTotals(self):
        '''Check that the values in the 'total' column of the 'totals' file are consistent'''
        test_logger.info("Checking values in 'total' column are consistent with data from source master for each file...")
        for filename in _df_master['totals']:
            if filename == '5':
                continue    # Analysis 5 "R0 Analysis" returns empty 'totals' dataframe
            assert list(_df_master['totals'][filename]['total'].values) == list(_df_dev['totals'][filename]['total'].values)

    def testConsistentValues(self):
        '''Check that the values in the 'value' column of the 'dynamics' file are consistent'''
        test_logger.info("Checking values in 'value' or 'R0' column are consistent with data from source master for each file...")
        for filename in _df_master['dynamics']:
            _col = 'R0' if filename == '5' else 'value' # Analysis 5 is R0 Analysis (different dataframe)
            assert list(_df_master['dynamics'][filename][_col].values) == list(_df_dev['dynamics'][filename][_col].values)
        
if __name__ in "__main__":
    unittest.main()
