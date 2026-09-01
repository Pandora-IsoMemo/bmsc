# Best model

Best model

## Usage

``` r
bestModel(models, loos, thresholdSE, ic)
```

## Arguments

- models:

  list of models fitted by
  [`constrSelEst`](https://pandora-isomemo.github.io/bmsc/reference/constrSelEst.md)
  function

- loos:

  list of model fits

- thresholdSE:

  numeric: How much standard errors in leave-one-out prediction
  performance can the sparse model be worse than the best model

- ic:

  information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"
