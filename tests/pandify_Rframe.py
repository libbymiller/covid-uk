import pandas as pd
import argparse
import pickle
import os
import subprocess
import logging
from rpy2.robjects.packages import importr

logging.basicConfig(level=logging.INFO)

qread = importr('qs').qread

class QStoPandaPickle(object):
    '''Read in a .qs file and convert it to a Pandas Dataframe then pickle it'''
    def __init__(self, in_file, output_dir):
        in_file = os.path.abspath(in_file)
        self._logger = logging.getLogger('QStoPandaPickle')
        _in_file_name = in_file.split('/')[-1] if '/' in in_file else in_file
        self._logger.info("Processing File, output will be '{}'".format(os.path.join(output_dir, _in_file_name.replace('.qs', '.pckl'))))
        self._out_dir = output_dir
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
            self._logger.error("Failed to write data to file '{}', this file is empty".format(_csv_name))
            raise RuntimeError

        return _csv_name

    def _dump_pd_dataframe(self):
        _pickle_file = self._input_file.split('/')[-1] if '/' in self._input_file else self._input_file
        _pickle_file = os.path.join(self._out_dir, _pickle_file.replace('csv', 'pckl'))
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
    parser.add_argument('file', help='Input R filename of type ".qs"')
    parser.add_argument('--out-dir', help='Output directory', default=os.getcwd())

    args = parser.parse_args()

    QStoPandaPickle(args.file, args.out_dir)
