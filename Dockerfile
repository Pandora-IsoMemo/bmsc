FROM inwt/r-shiny:4.3.2

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/*

RUN installPackage \
    mice \
    rstan \
    rstantools \
    StanHeaders

ADD . .

RUN Rscript -e "rstantools::rstan_config()"

RUN installPackage
