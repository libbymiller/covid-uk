FROM ubuntu:18.04
# Turn off interactive options for tzdata
ENV DEBIAN_FRONTEND=noninteractive
RUN apt update
RUN apt upgrade -y
RUN apt update --fix-missing
RUN apt install software-properties-common gnupg2 -y
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/'
RUN apt update
RUN apt install r-base-dev r-doc-html r-base-core r-recommended -y
RUN apt install libxml2-dev libssl-dev libcurl4-openssl-dev libnlopt-dev curl libgsl-dev libhdf5-dev libudunits2-dev libgdal-dev -y
RUN apt install apt-transport-https software-properties-common -y
RUN R -e "install.packages(\"remotes\")"
RUN R -e "library(remotes)"
RUN R -e "remotes::install_github(\"traversc/qs@legacy\")"
RUN R -e "install.packages(\"curl\")"
RUN R -e "install.packages(\"httr\")"
RUN R -e "install.packages(\"rvest\")"
RUN R -e "install.packages(\"rlang\")"
RUN R -e "install.packages(\"stringr\")"
RUN R -e "install.packages(\"data.table\")"
RUN R -e "install.packages(\"ggplot2\")"
RUN R -e "install.packages(\"lubridate\")"
RUN R -e "install.packages(\"nloptr\")"
RUN R -e "install.packages(\"HDInterval\")"
RUN R -e "install.packages(\"cowplot\")"
RUN R -e "install.packages(\"testit\")"
RUN R -e "install.packages(\"readxl\")"
RUN R -e "install.packages(\"ini\")"
RUN R -e "install.packages(\"tidyverse\")"
RUN R -e "install.packages(\"lubridate\")"
RUN R -e "install.packages(\"testit\")"
RUN R -e "install.packages(\"RcppGSL\")"
RUN R -e "install.packages(\"reticulate\")"
RUN R -e "install.packages(\"snakecase\")"
RUN R -e "remotes::install_github(\"ScottishCovidResponse/SCRCdataAPI\")"
RUN apt install libgsl-dev -y
RUN R -e "install.packages(\"RcppGSL\")"
RUN apt install libnlopt-dev -y
RUN adduser coviduk
RUN apt install git -y
RUN apt install vim emacs -y
RUN apt install python3-pip -y
RUN mkdir -p /home/coviduk
RUN pip3 install pandas rpy2 nose h5py
RUN git clone https://github.com/ScottishCovidResponse/covid-uk.git /home/coviduk/covid-uk
WORKDIR /home/coviduk
WORKDIR /home/coviduk/covid-uk
RUN git checkout dev
RUN rm -f /usr/bin/python
RUN ln -s /usr/bin/python3 /usr/bin/python
RUN python -m pip install pyyaml
RUN pip3 install -r /home/coviduk/covid-uk/SCRC/Python/requirements.txt
RUN chown -R coviduk:coviduk /home/coviduk
RUN chmod 755 /home/coviduk
USER coviduk
RUN Rscript run_model.R 1
ENTRYPOINT ["/bin/bash"]
