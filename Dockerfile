FROM inwt/r-shiny:4.3.2

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    && apt-get autoremove -y \
    && apt-get autoclean -y \
    && rm -rf /var/lib/apt/lists/*

# add Pandora Repository
RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

RUN installPackage \
    mice \
    rstan \
    rstantools \
    StanHeaders

ADD . .

RUN Rscript -e "rstantools::rstan_config()"

RUN installPackage
