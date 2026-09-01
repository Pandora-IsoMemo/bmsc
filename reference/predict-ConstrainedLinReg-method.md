# Compute predictions from constraint estimation model

Computes prediction from model of class
[`ConstrainedLinReg`](https://pandora-isomemo.github.io/bmsc/reference/ConstrainedLinReg-class.md)
and a data.frame.

## Usage

``` r
# S4 method for class 'ConstrainedLinReg'
predict(object, newdata, samples = FALSE)
```

## Arguments

- object:

  Model of class
  [`ConstrainedLinReg`](https://pandora-isomemo.github.io/bmsc/reference/ConstrainedLinReg-class.md)

- newdata:

  data.frame containing all variables that appear in the model formula

- samples:

  boolean return samples of predictions?

## Value

Numeric vector of predictions. For observations with missing values on
the explanatory variables, a prediction of `NA` is returned.
