FROM ghcr.io/pandora-isomemo/base-image:latest

RUN installPackage pkgbuild

RUN Rscript -e "install.packages('https://cran.r-project.org/src/contrib/Archive/mice/mice_3.14.0.tar.gz', repos = NULL)"

ADD . .

RUN installPackage

RUN Rscript -e "pkgbuild::compile_dll(); devtools::document()"

RUN installPackage
