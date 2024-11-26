FROM inwt/r-shiny:4.3.2

RUN apt-get update \
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

COPY . .

RUN Rscript -e "rstantools::rstan_config()"

RUN installPackage
