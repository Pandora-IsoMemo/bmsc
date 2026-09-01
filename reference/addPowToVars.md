# Add exponent to a vector of variables

Remark: Since this function is to be used only within
[`createFormula`](https://pandora-isomemo.github.io/bmsc/reference/createFormula.md),
the validity of the input is not checked here but in
[`createFormula`](https://pandora-isomemo.github.io/bmsc/reference/createFormula.md).

## Usage

``` r
addPowToVars(vars, power)
```

## Arguments

- vars:

  character: variable names

- power:

  integer: exponent

## Value

Vector of same length as `vars`

## Examples

``` r
BMSC:::addPowToVars(c("x1", "x2"), 2)
#> [1] "I(x1^2)" "I(x2^2)"
```
