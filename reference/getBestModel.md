# Get Best Model after Models Selection

Get Best Model after Models Selection

## Usage

``` r
getBestModel(models, thresholdSE = 1, ic = "Loo")
```

## Arguments

- models:

  list of models fitted by
  [`constrSelEst`](https://pandora-isomemo.github.io/bmsc/reference/constrSelEst.md)
  function

- thresholdSE:

  numeric: How much standard errors in leave-one-out prediction
  performance can the sparse model be worse than the best model

- ic:

  information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"

## Value

The best sparse model concerning leave-one-out performance within a
threshold
