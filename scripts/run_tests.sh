#!/bin/bash

RUNTESTS=true
EXE_R_SCRIPT=$PWD/scripts/UK.R

python -c "import tulip"

if [[ $? != "0" ]]; then
	echo ""
	echo "WARNING: Pandas not found in Python installation, will run simulations but not tests"
	echo ""
	RUNTESTS=false
fi

echo "==== Running Covid-UK simulation on current branch with 1 stochastic realisation for testing ===="
for i in 1 2.1 2.2 3 4 5 6; do echo " * === Running Stage $i ===" && $EXE_R_SCRIPT $i 1; done

if [ $RUNTESTS ]; then	
    echo "==== Adding 'test' to filenames of output .qs files and moving to directory 'data' ===="
    rename .qs test.qs *.qs
    mv *.qs data/
    echo "==== Creating directory 'tests/test_data/test', converting dataframes then moving output files to this location ===="
    mkdir -p tests/test_data/test
    python3 tests/pandify_Rframe.py --in-dir data/ --out-dir tests/test_data/test
    echo "==== Running software tests ===="
    nosetests . --nologcapture

fi
