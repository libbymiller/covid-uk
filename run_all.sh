export CURRENT_BRANCH=$(git symbolic-ref --short -q HEAD)

echo "==== Running Covid-UK simulation on branch '$CURRENT_BRANCH' with 1 stochastic realisation for testing ===="
for i in 1 2.1 2.2 3 4 5 6; do echo " * === Running Stage $i ===" && Rscript UK.R $i 1; done
echo "==== Adding '${CURRENT_BRANCH}' to filenames of output .qs files and moving to directory 'data' ===="
rename .qs ${CURRENT_BRANCH}.qs *.qs
mv *.qs data/
echo "==== Creating directory 'tests/test_data/${CURRENT_BRANCH}', converting dataframes then moving output files to this location ===="
mkdir -p tests/test_data/${CURRENT_BRANCH}
python3 tests/pandify_Rframe.py --in-dir data/ --out-dir tests/test_data/${CURRENT_BRANCH}
echo "==== Running software tests ===="
nosetests . --nologcapture
