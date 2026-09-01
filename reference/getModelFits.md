# Get Model Fits

Get Model Fits

## Usage

``` r
getModelFits(
  models,
  y = NULL,
  newdata = NULL,
  cores = getOption("mc.cores", 4)
)
```

## Arguments

- models:

  list of models fitted by
  [`constrSelEst`](https://pandora-isomemo.github.io/bmsc/reference/constrSelEst.md)
  function

- y:

  response variable

- newdata:

  data.frame containing all variables that appear in the model formula

- cores:

  number of cores to use, compare
  [`loo`](https://mc-stan.org/loo/reference/loo.html)
