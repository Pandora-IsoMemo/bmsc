FROM ghcr.io/pandora-isomemo/base-image:latest

RUN installPackage pkgbuild

RUN Rscript -e "install.packages('https://cran.r-project.org/src/contrib/Archive/mize/mize_0.2.3.tar.gz', repos = NULL)"

ADD . .

RUN Rscript -e "pkgbuild::compile_dll(); devtools::document()"

RUN installPackage
