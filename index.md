# BMSC (constraint-estimation)

## Overview

BMSC provides Bayesian model selection and constrained coefficient
estimation for linear regression models. It supports variable selection
over main effects, interactions, and polynomial terms, and can
incorporate user-defined constraints on regression coefficients. Models
are fitted with Stan via `rstan`, with helper functions for formula
construction, missing-data handling, model comparison, and prediction.

## Documentation

- <https://pandora-isomemo.github.io/bmsc/>

## Release notes

- see `NEWS.md`

## Local Installation

- to re-generate `R/stanmodel.R` and the C++ Source Code in src use
  [`rstantools::rstan_config()`](https://mc-stan.org/rstantools/reference/rstan_config.html)
- Next, execute the `./createMakeVars` script. This step is essential to
  ensure `devtools::check()` functions correctly.
- After that you can install and compile the package
  e.g. `devtools::load_all()`

## Notes for developers

When adding information to the *help* sites, *docstrings* or the
*vignette* of this package, please update documentation locally as
follows. The documentation of the main branch is built automatically via
GitHub Actions.

``` r

devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```
