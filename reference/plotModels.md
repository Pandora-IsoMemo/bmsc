# Plot model errors with errorbars

Plot model errors with errorbars

## Usage

``` r
plotModels(
  datPlot,
  colours,
  thresholdSE,
  ic,
  ylab = NULL,
  xlab = NULL,
  aSize = 12,
  lSize = 14,
  tAngle = 30
)
```

## Arguments

- datPlot:

  data.frame with prepared plot data

- colours:

  character: colour(s) for the points, bars and x-axis labels

- thresholdSE:

  numeric: Factor multiplied with standard error to obtain ends of error
  bars

- ic:

  information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"

- ylab:

  character: y title

- xlab:

  character: x title

- aSize:

  character: axis label size

- lSize:

  character: axis title label size

- tAngle:

  numeric: x axis text angle
