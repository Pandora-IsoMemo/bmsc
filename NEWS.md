# BMSC version 26.08.0

## Updates
- Regenerated Stan model export sources/headers (stanc3 v2.32.2 output) for `linReg` and `linRegHorseShoe`.
- Expanded and organized `.Rbuildignore`, `.gitignore`, and `.dockerignore` entries to reduce accidental inclusion of local/CI/build artifacts.

# BMSC version 26.07.0

## Updates
- Updated base image in Dockerfile to r-shiny:4.4.1

# BMSC version 24.11.1

## Bugfixes
- fix issues with RCMD check (#8)
- add `cores` argument to `getModelFits` for running the test
- adjust CI Pipeline & Dockerfile to generate the cpp code in the container

# BMSC version 23.07.1.2

## Bugfixes
- missing categorical data imputation

# BMSC version 23.07.1

## New Features
- Bayesian R-squared (follwing https://avehtari.github.io/bayes_R2/bayes_R2.html)

# BMSC version 23.01.1

## New Features
- Add inverse exponents as features. Now one can add x^-1, x^-2,.. as potential modelling features
- Model averaging added. Now models can be averaged by a criterion (AIC, AICc, WAIC, logLik, BIC and Loo)
- Imputation of missing values added (multiple imputation via the mice package)
