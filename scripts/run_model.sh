#!/bin/bash

EXE_R_SCRIPT="Rscript $PWD/scripts/UK.R"

NARGS=$#
RUNARGS=""

function print_help()
{
    echo "" 
    echo "Usage:"
    echo "./run_model.sh <analysis-id> <n-realisations> [--parameters] [--help] [--settings] [--coviduk] [--dump] [--contact-matrices]"
    echo ""
    echo "  analysis-id     Identifier for the analysis to be run"
    echo "  n-realisations  Number of stochastic realisations"
    echo ""
    echo "   --parameters=<params>             Parameters INI file"
    echo "   --settings=<settings>             Settings INI file"
    echo "   --help                            Print this help string"
    echo "   --dump                            Dump parameters after initialisation and exit (for testing)"
    echo "    --contact-matrices=<matrix_file>  Location of contact matrices RDS file"
    echo ""
    exit 0
}

if [ $NARGS -eq 0 ]; then
    print_help
fi

for i in "$@"; do
    if [ "$i" == "--help" ]; then
        print_help
    fi

    if [ "$i" == "--parameters" ] || [ "$i" == "--settings" ] || [ "$i" == "--coviduk" ] || [ "$i" == "--contact-matrices   " ]; then
        RUNARGS = "${RUNARGS} $i ${i+1}"
    fi

    if [ "$i" == "--dump" ]; then
        RUNARGS = "${RUNARGS} $i"
    fi
done
exit 0
$EXE_R_SCRIPT $1 $2 $RUNARGS