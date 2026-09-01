# Plot errors of all models

This plot is automatically produced with the execution of
[`getBestModel`](https://pandora-isomemo.github.io/bmsc/reference/getBestModel.md).

## Usage

``` r
plotModelFit(
  models,
  fits = NULL,
  thresholdSE = 1,
  markBestModel = TRUE,
  ic = "Loo",
  ylab = NULL,
  xlab = NULL,
  aSize = 12,
  lSize = 14,
  tAngle = 30
)
```

## Arguments

- models:

  List with models of class
  [`ConstrainedLinReg`](https://pandora-isomemo.github.io/bmsc/reference/ConstrainedLinReg-class.md)

- fits:

  Optional list with model fit measures from getModelFits() - function

- thresholdSE:

  numeric: Factor multiplied with standard error to obtain ends of error
  bars

- markBestModel:

  boolean: highlight position of the best model in the model list

- ic:

  information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"

- ylab:

  character: y title

- xlab:

  character: x title

- aSize:

  numeric: axis label size

- lSize:

  numeric: axis title label size

- tAngle:

  numeric: x axis text angle
