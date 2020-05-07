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

import unittest     # Use Unit Test to automate tests
import logging      # Custom loggers for tests
import pandas       # Read in R dataframes into Pandas DataFrames
import glob         # Read in dataframes from each branch output folder
import os
import pickle
from git import Repo
from collections import Counter

git_branch = Repo(os.getcwd()).active_branch

logging.basicConfig(level=logging.INFO)

def _get_df(file_addrs):
    frames = {}
    for F in file_addrs:
        with open(F, 'rb') as f:
            frames[F.split('/')[-1].split('-')[1]] = pickle.load(f)

    return frames
baseline_data = glob.glob(os.path.join(os.getcwd(), 'tests', 'test_data', '*master.pckl'))
_df_master = _get_df(baseline_data)
_df_dev = _get_df(i.replace('master', str(git_branch)) for i in baseline_data)

class TestCOVIDUK(unittest.TestCase):
    def testAllDataFramesPresent(self):
        assert all(i in _df_dev for i in _df_master)

    def testConsistentEntriesPerScenario(self):
        for filename in _df_master:
            _count_master = Counter(_df_master[filename]['scenario'].values)
            _count_dev = Counter(_df_dev[filename]['scenario'].values)

            for key in _count_master:
                assert _count_master[key] == _count_dev[key]

    def testConsistentTotals(self):
        assert list(_df_master['totals']['total'].values) == list(_df_dev['totals']['total'].values)

    def testConsistentValues(self):
        assert list(_df_master['dynamics']['value'].values) == list(_df_dev['dynamics']['value'].values)
        

if __name__ in "__main__":
    if not git_branch == 'master' and not baseline_data:
        print('Unable to find data for master branch')
        raise FileNotFoundError
    unittest.main()
