##############################################################################
#                                                                            #
#                   COVID-UK QS to PANDAS DATAFRAME                          #
#                                                                            #
#   This script prepares the data collected from running:                    #
#                                                                            #
#   Rscript UK.R <case> 1                                                    #
#                                                                            #
#   on all branches/merge-requests under consideration towards testing.      #
#                                                                            #
#   NOTE: This script should be run from the repository root directory       #
#                                                                            #
#   @author     :   K. Zarebski (UKAEA)                                      #
#   @date       :   last modified 2020-05-11                                 #
#                                                                            #
##############################################################################

import pandas as pd
import argparse
import pickle
import os
import glob
import subprocess
import logging
from rpy2.robjects.packages import importr

logging.basicConfig(level=logging.INFO)

qread = importr('qs').qread

class QStoPandaPickle(object):
    '''Read in a .qs file and convert it to a Pandas Dataframe then pickle it'''
    def __init__(self, in_dir, output_dir):
        self._logger = logging.getLogger('QStoPandaPickle')
        self._in_dir = in_dir
        self._out_dir = output_dir
        self._process_directory()


    def _process_directory(self):
        '''Read in all .qs files from the selected input directory and convert'''
        _search_str = os.path.join(self._in_dir, '*.qs')
        self._logger.info('Searching for files in: {}'.format(_search_str))
        _file_list = glob.glob(_search_str)

        if not _file_list:
            self._logger.error('No input files were found')
            raise FileNotFoundError

        for f in _file_list:
            self._process_file(f)

    def _process_file(self, in_file):
        in_file = os.path.abspath(in_file)
        _in_file_name = in_file.split('/')[-1] if '/' in in_file else in_file
        self._logger.info("Processing File, output will be '{}'".format(
            os.path.join(self._out_dir, _in_file_name.replace('.qs', '.pckl'))))
        self._input_file = self._qs2csv(in_file)
        self._dump_pd_dataframe()

    def _qs2csv(self, in_file):
        '''Convert .qs file to a .csv file to be read by Pandas'''
        if not os.path.exists(in_file):
            self._logger.error("Requested file '{}' does not exist!".format(in_file))
            raise FileNotFoundError

        self._logger.info("Reading in QS R data file '{}'".format(in_file))

        _temp = qread(in_file)

        self._logger.info('DATA HEAD:\n'+str(_temp.head()))

        _csv_name = in_file.replace('qs', 'csv')

        self._logger.info("Writing data to CSV file")

        _temp.to_csvfile(_csv_name)

        if os.stat(_csv_name).st_size == 0:
            self._logger.error("Failed to write data to file "+
                "'{}', this file is empty".format(_csv_name))
            raise RuntimeError

        return _csv_name

    def _dump_pd_dataframe(self):
        _pickle_file = self._input_file.split('/')[-1] if '/' in self._input_file else self._input_file
        _pickle_file = os.path.join(self._out_dir, _pickle_file.replace('.csv', '.pckl'))
        self._logger.info('Creating Pandas DataFrame')
        _pandas_df = pd.read_csv(self._input_file)
        self._logger.info('Pickling result')
        with open(_pickle_file, 'wb') as f:
            pickle.dump(_pandas_df, f)
        if not os.path.exists(_pickle_file):
            self._logger.error("Failed to write data to Pickle file '{}'".format(_pickle_file))
            raise FileNotFoundError

        subprocess.check_call('rm {}'.format(self._input_file), shell=True)

        self._logger.info('Pickling of Data Complete:OUT: {}'.format(_pickle_file))


if __name__ in "__main__":
    parser = argparse.ArgumentParser('PandifyRFrame')
    parser.add_argument('--in-dir', help='Input directory', default=os.getcwd())
    parser.add_argument('--out-dir', help='Output directory', 
            default=os.path.join(os.getcwd(), 'tests', 'test_data'))

    args = parser.parse_args()

    QStoPandaPickle(args.in_dir, args.out_dir)
