# Changelog

## BMSC version 26.09.1

### Bugfixes

- Added a `configure.win` script (identical to `configure`) so Windows
  source installations also regenerate `src/Makevars`; R does not fall
  back to `configure` on Windows when `configure.win` is missing, which
  previously caused `stan/version.hpp`, `tbb/tbb_stddef.h`, and
  `stan_meta_header.hpp` build failures on Windows.

## BMSC version 26.09.0

### Bugfixes

- Added an install-time `configure` script so source installations
  regenerate `src/Makevars` with the local `StanHeaders`, `rstan`, and
  `RcppParallel` paths instead of using stale paths from the build
  machine.
- Stopped tracking and shipping generated `src/Makevars`, preventing
  local development commands from leaving machine-specific paths in
  version control.
- Declared `RcppParallel` in `LinkingTo` and updated CI/Docker setup to
  rely on the package `configure` script for Makevars generation.
- Updated Stan model array declarations to the current syntax required
  by stanc3/rstan during
  [`rstantools::rstan_config()`](https://mc-stan.org/rstantools/reference/rstan_config.html).

## BMSC version 26.08.0

### Updates

- Regenerated Stan model export sources/headers (stanc3 v2.32.2 output)
  for `linReg` and `linRegHorseShoe`.
- Expanded and organized `.Rbuildignore`, `.gitignore`, and
  `.dockerignore` entries to reduce accidental inclusion of
  local/CI/build artifacts.

## BMSC version 26.07.0

### Updates

- Updated base image in Dockerfile to r-shiny:4.4.1

## BMSC version 24.11.1

### Bugfixes

- fix issues with RCMD check (#8)
- add `cores` argument to `getModelFits` for running the test
- adjust CI Pipeline & Dockerfile to generate the cpp code in the
  container

## BMSC version 23.07.1.2

### Bugfixes

- missing categorical data imputation

## BMSC version 23.07.1

### New Features

- Bayesian R-squared (follwing
  <https://avehtari.github.io/bayes_R2/bayes_R2.html>)

## BMSC version 23.01.1

### New Features

- Add inverse exponents as features. Now one can add x^-1, x^-2,.. as
  potential modelling features
- Model averaging added. Now models can be averaged by a criterion (AIC,
  AICc, WAIC, logLik, BIC and Loo)
- Imputation of missing values added (multiple imputation via the mice
  package)
