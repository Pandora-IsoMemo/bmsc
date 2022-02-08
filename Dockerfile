FROM bmsc-base:latest

ADD . .

RUN Rscript -e "pkgbuild::compile_dll(); devtools::document()"

RUN installPackage
