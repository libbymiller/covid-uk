for i in 1 2.1 2.2 3 4 5 6; do Rscript UK.R $i $1; done
nosetests .
