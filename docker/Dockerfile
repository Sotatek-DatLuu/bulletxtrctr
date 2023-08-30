FROM rocker/r-base:4.2.3

WORKDIR bulletxtrctr-master

RUN apt-get update && \
    apt-get install -y libcurl4-openssl-dev libssl-dev

COPY . .

RUN Rscript install_packages.R

