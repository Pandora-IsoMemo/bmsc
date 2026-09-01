# Prepare data to plot model fit

Prepare data to plot model fit

## Usage

``` r
prepPlotData(fits, modelNames, thresholdSE, ic = "Loo")
```

## Arguments

- fits:

  Optional list with model fit measures from getModelFits() - function

- modelNames:

  Names for the models in the same order as they appear in `Loos`

- thresholdSE:

  numeric: Factor multiplied with standard error to obtain ends of error
  bars

- ic:

  information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"

## Value

A data.frame with the columns `Estimate` (Estimate of the Looic), `SE`,
`model`, `lower`, and `upper`
