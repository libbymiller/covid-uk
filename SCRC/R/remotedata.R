library(reticulate)
use_python("/home/kristian/venvs/lshtm/bin/python")

covid_uk = '.'

source_python(file.path(covid_uk, 'SCRC/Python/data.py'))

py_run_string('data_store = Datastore("config.yaml")')
py_run_string('test_table = data_store.read_table("human/human/age_var_symptomatic_rates")')