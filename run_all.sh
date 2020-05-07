export CURRENT_BRANCH=$(git symbolic-ref --short -q HEAD)

echo "==== Running Covid-UK simulation on branch '$CURRENT_BRANCH' with 1 stochastic realisation for testing ===="
for i in 1 2.1 2.2 3 4 5 6; do echo "==== Running Stage $i ====" && Rscript UK.R $i 1; done
rename .qs ${CURRENT_BRANCH}.qs *.qs 
nosetests .

