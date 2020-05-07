echo "==== Running Covid-UK simulation with 1 stochastic realisation for testing ===="
for i in 1 2.1 2.2 3 4 5 6; do echo "==== Running Stage $i ====" && Rscript UK.R $i 1; done
nosetests .
