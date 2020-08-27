# Scottish COVID Response Consortium Adaptation of the LSHTM COVID-UK Model
![LSHTM Model](https://github.com/ScottishCovidResponse/covid-uk/workflows/LSHTM%20Model/badge.svg)

This is the SCRC version of the COVID-UK model by the London School of Hygiene & Tropical Medicine. The model is described as:

*[A] Stochastic age-structured model of SARS-nCoV-2 transmission for UK scenario projections*.

The Wiki for this project can be found on this repository [here](https://github.com/ScottishCovidResponse/covid-uk/wiki).

## SCRC Implementation

The SCRC implementation of the LSHTM model is designed to read data and parameters from external sources as opposed to using the included/built in parameters included within the original. Ultimately the wrapper will allow the model to be run either locally using these existing files, or by connecting to an external API which will provide the values.

### Removed Features

As this is important I have put this at the top of the README, the following features have been removed possibly pending future review:

* **Varying the seeding time depending on whether the select region is a London borough.** The main issue with this is the method use assumes the data form is absolute (i.e. you want to run on every UK region simultaneously, when in the SCRC case we are initially only running on Scottish data).

### API Run Requirements

In order to run the API call version of the model you will need to install the R wrapper for Python:
```
install.packages("reticulate")
```
and ensure your python environment has installed the libraries within `SCRC/Python/requirements.txt`. This will also install the data pipeline API.

### Model Run

To run the model using this method use the new `run_model.R` script included. The script takes a single required argument which is the number of stochastic realisations. The model can either be run using the SCRC API (included within the requirements above) or locally using the original datasets that came with it. A full list of available options is given by running:

```bash
Rscript run_model.R --help
```

Note that for all model runs the compulsory argument `<n-runs>` must come first, this is due to the limitations of command arguments parsed via R.

#### Local Running

A local run in started by including the `--local` flag:

```bash
Rscript run_model.R <n-realisations> --local
```
this will produce outputs in the `output` folder. The model will read parameters locally from the `configuration/parameters.ini` file and from the sources contained within the `data` folder.

The seeding parameter `seed/value` can be set within the parameters fileto a value or left at `-1` to indicate time based seeding.

#### Running with the API

Before running the model the API is called to download the data directories to the local system. If a `metadata.yaml` file is not found in the config file path then this process is performed automatically prior to a model run. However if you wish to do this manually the python command requires a single argument pointing to the location of a `config.yaml` file (an example is included in the `SCRC/pipeline_data` directory):

```
python -m data_pipeline_api.registry.download --config <path-to-config-file>
```

The model is run as:

```bash
Rscript run_model.R <n-realisations>
```

Note if the default `config.yaml` file is not being used the argument `--config=<path-to-config-file>` can be included.


### Outputs

Outputs vary depending on the mode of execution. For API based and local runs two files are produced in the directory `output` which contain R objects describing totals and dynamics identical to the original LSHTM version.

In the case of an API run an additional folder `lshtm_outputs` is created in the API data directory (the directory as stated in the input `config.yaml`, in the case of the default file, this is found in `SCRC/pipeline_data`). Within this directory are subdirectories with HDF5 files which contain tables summarising the data within the `.qs` files in a more easily interprettable form. `cases_deaths` containing a summary of the total number of cases and deaths, `dynamics_summary` summarising the flow of individuals between epidemiological states and `dynamics_time_series` showing how these states change after each time step.

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

## Original Model

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

## Running on Docker (Recommended)

The repository contains a Dockerfile which can be used to build an image, from within the repository run:

`docker build -t coviduk .`

This will create a new container called `coviduk` which can be started and stopped using:

`docker start coviduk`
`docker stop coviduk`

Open a terminal within the container by using:

`docker attach coviduk`

you can then run all the commands listed above from within the `/home/coviduk/covid-uk` folder.

### Reference

[Davies NG et al. The effect of non-pharmaceutical interventions on COVID-19 cases, deaths and demand for hospital services in the UK: a modelling study. CMMID COVID-19 working group pre-print, 2020.](https://cmmid.github.io/topics/covid19/control-measures/uk-scenario-modelling.html)
