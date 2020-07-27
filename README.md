# covid-uk
![LSHTM Model](https://github.com/ScottishCovidResponse/covid-uk/workflows/LSHTM%20Model/badge.svg)

Stochastic age-structured model of SARS-nCoV-2 transmission for UK scenario projections.

## Removed Features

As this is important I have put this at the top of the README, the following features have been removed possibly pending future review:

* **Varying the seeding time depending on whether the select region is a London borough.** The main issue with this is the method use assumes the data form is absolute (i.e. you want to run on every UK region simultaneously, when in the SCRC case we are initially only running on Scottish data).

## Quick start guide

### Installing dependencies for Mac OS

You will need to install gfortran binaries from here: https://github.com/fxcoudert/gfortran-for-macOS/releases

Once installed, run `gcc --version` in terminal to get your current version, e.g. `Target: x86_64-apple-darwin18.8.2.0`. Then run below in terminal to add library path for R:

`cd ~
mkdir .R
cd .R
echo FLIBS=-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm >> Makevars
`

Finally, install nlopt: `brew install nlopt`

## SCRC Implementation

The SCRC implementation of the LSHTM model is designed to read data and parameters from external sources as opposed to using the included/built in parameters included within the original. Ultimately the wrapper will allow the model to be run either locally using these existing files, or by connecting to an external API which will provide the values.

### API Run Requirements

In order to run the API call version of the model you will need to install the R wrapper for Python:
```
install.packages("reticulate")
```
and ensure your python environment has installed the libraries within `SCRC/Python/requirements.txt`. This will also install the data pipeline API.

### Model Run

To run the model using this method use the new `run_model.R` script included. The script takes a single required argument which is the number of stochastic realisations. To run locally (currently the only option available):

```
Rscript run_model.R <n-realisations> --local
```
this will produce outputs in the `output` folder. The model will read parameters locally from the `configuration/parameters.ini` file and from the sources contained within the `data` folder.

The seeding parameter `seed/value` can be set to a value or left at `-1` to indicate time based seeding.

### Plotting

To produce plots and numerical data use the included `plot_results.R` script stating the prefix of the run, if you are using the local version defaults this will be:

`Rscript plot_results.R run-Other-<n-realisations>`

the exact format should be determinable from the `.qs` output files in the `outputs` directory. This script will create a PDF and CSV file with the results.

### Testing

As the model itself (present as the RCpp function `cm_backend_simulate`) has not been altered testing is only needed on the form of the parameters being fed into the model matching those present during the run of the original form. Contained within the `testing/regression` folder is a script `test_params.R` which compares the parameters. In order to perform the test you will firstly need to run the model in "dump" mode which will stop execution prior to simulation and dump the parameters to the output folder, only one realisation needs to be run:

```
Rscript run_model.R 1 --local --dump
```
then run:

```
Rscript tests/regression/test_params.R
```

the script will compare the latest dumps with the files contained within the `tests/test_data/baseline` folder.

## Original Implementation

### Guide to files

Main parameter setting and model run is in `scripts/UK.R` however the model is using the new `scripts/run_model.sh` script. Output collation and plotting functions are in `UK-view.R`. Underlying model code is in `covidm` folder.

Some parameters are now set to be read from external INI files which are located in the `configuration` folder, these include distribution definitions and
time steps.

A full print out of the run options can be viewed by running:

```
./script/run_model.sh --help
```

and include the ability to the location of these parameter file, as well as other data, and dump the parameters after initialisation for testing.

To run the model in it's simplest form simply run:

```
./scripts/run_model.sh 1 50
```

Here, 1 is the number for the analysis you want to run (1, 2.1, 2.2, 3, 4, 5, or 6). 50 is the number of stochastic realisations to run.

1 - 12 week interventions

2.1 - national triggering

2.2 - local triggering

3 - lockdowns

4 - elder care during school closures

5 - R0 analysis

6 - leisure and sports analyses

For 50 runs, each set takes about 6-16 hours on a current laptop.

## Testing After Development

The R scripts require libaries to be installed (see the `Dockerfile` which contains a list of bash commands showing installation of these) and have been confirmed to run with Rscript `v4.0.0`.

Included within the repository is a set of tests which are designed to be run from a development branch, they compare the results obtained with the current scripts to those obtained with running the original source code from the parent repository. It is recommended you run within the root of this repository:

`./scripts/run_tests.sh`

which will run the relevant R code followed by the Python nose tests.
The tests require you to install the modules contained within the `tests/requirements.txt` file.

## Running on Docker (Recommended)

The repository contains a Dockerfile which can be used to build an image, from within the repository run:

`docker build -t coviduk .`

a container can then be created in which you should mount this repository:

`docker run --name coviduk --mount type=bind,source=/path/to/this/repository,target=/home/coviduk/covid-uk -ti coviduk:latest`

This will create a new container called `coviduk` which can be started and stopped using:

`docker start coviduk`
`docker stop coviduk`

Open a terminal within the container by using:

`docker attach coviduk`

you can then use the `bash run_all.sh` command.

Using a Docker allows you to make changes in the repository on your local machine, then test them by launching the container and running the scripts within the already prepared environment.

### Reference

[Davies NG et al. The effect of non-pharmaceutical interventions on COVID-19 cases, deaths and demand for hospital services in the UK: a modelling study. CMMID COVID-19 working group pre-print, 2020.](https://cmmid.github.io/topics/covid19/control-measures/uk-scenario-modelling.html)
