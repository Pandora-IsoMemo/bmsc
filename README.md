# BMSC (constraint-estimation)

<!-- badges: start -->
[![R-CMD-check](https://github.com/Pandora-IsoMemo/bmsc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/bmsc/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/Pandora-IsoMemo/bmsc/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Pandora-IsoMemo/bmsc/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

## Overview

BMSC provides Bayesian model selection and constrained coefficient estimation for
linear regression models. It supports variable selection over main effects,
interactions, and polynomial terms, and can incorporate user-defined constraints
on regression coefficients. Models are fitted with Stan via `rstan`, with helper
functions for formula construction, missing-data handling, model comparison, and
prediction.

## Documentation
- https://pandora-isomemo.github.io/bmsc/

## Release notes

- see `NEWS.md`

## Local Installation

* to re-generate `R/stanmodel.R` and the C++ Source Code in src use `rstantools::rstan_config()`
* `src/Makevars` is generated automatically during source installation via the
  package `configure` script. For local development, you can run `./configure`
  or `./createMakeVars` manually before `devtools::check()` if needed. The
  generated file is ignored and should not be committed.
* After that you can install and compile the package e.g. `devtools::load_all()`

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is built automatically via GitHub Actions.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```