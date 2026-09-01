# Exclude rows with missing data on predictor variables

Rows with missing values on predictor variables are excluded. An unused
column for the dependent variable is added to avoid errors.

## Usage

``` r
prepDatForPredict(formula, newdata, catVars)
```

## Arguments

- formula:

  Model formula

- newdata:

  data.frame containing all variables that appear in the model

- catVars:

  categorical variables in the model

## Value

Object of class [`na.exclude`](https://rdrr.io/r/stats/na.fail.html)

## Details

A column of ones for the dependent variable is added. Otherwise
[`model.matrix`](https://rdrr.io/r/stats/model.matrix.html) tries to
take it from the formula's environment, which is the original data. This
usually results in an error due to unequal variable length. This column
is however not used.
