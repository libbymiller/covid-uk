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
RUN adduser coviduk
RUN apt install git -y
RUN apt install vim emacs -y
RUN apt install python3-pip -y
RUN mkdir -p /home/coviduk
RUN python3 -m pip install pandas rpy2 nose h5py
RUN apt install libgit2-dev -y
RUN git clone https://github.com/ScottishCovidResponse/covid-uk.git /home/coviduk/covid-uk
WORKDIR /home/coviduk
WORKDIR /home/coviduk/covid-uk
RUN Rscript /home/coviduk/covid-uk/SCRC/R/requirements.R --quiet --ncpus=2
RUN python3 -m pip install pyyaml dataclasses locales
ENV LC_ALL=C.UTF-8
ENV export LANG=C.UTF-8
RUN python3 -m pip install -r /home/coviduk/covid-uk/SCRC/Python/requirements.txt
RUN chown -R coviduk:coviduk /home/coviduk
RUN chmod 755 /home/coviduk
USER coviduk
RUN Rscript run_model.R 1
ENTRYPOINT ["/bin/bash"]
