FROM ghcr.io/pandora-isomemo/base-image:latest

RUN installPackage pkgbuild

ADD . .

RUN Rscript -e "pkgbuild::compile_dll(); devtools::document()"

RUN installPackage
