##############################################################################
#                                                                            #
#                           COVID-UK MODEL TESTING                           #
#                                                                            #
# Regression tests to ensure consistency between versions of the covid-uk    #
# adapted for comparison by the SCRC.                                        #
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


logging.basicConfig(level=logging.INFO)

baseline_data = os.path.join(os.getcwd(), 'testing', 'test_data', 'baseline_results.pckl')

class TestCOVIDUK(unittest.TestCase):
    def __init__(self):
        self._df0 = self._get_df(baseline_data)

    def _get_df(self, file_addr):
        with open(file_addr) as f:
            return pickle.load(f)

    def testDefaultDataFrames(self, data_frames):
        
