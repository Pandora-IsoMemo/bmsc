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
  package `configure` script (`configure.win` on Windows). For local
  development, you can run `./configure` (or `configure.win` on Windows) or
  `./createMakeVars` manually before `devtools::check()` if needed. The
  generated file is ignored and should not be committed.
* After that you can install and compile the package e.g. `devtools::load_all()`

### Troubleshooting

If a source install fails with errors like `stan/version.hpp`,
`tbb/tbb_stddef.h`, or `stan_meta_header.hpp` not found, `src/Makevars` was not
regenerated for your system. Run `configure` (`configure.win` on Windows)
manually from the package root, then reinstall/reload the package.

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is built automatically via GitHub Actions.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```