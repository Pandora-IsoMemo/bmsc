# Create polynomial of degree `maxExponent` from variable names

Remark: Since this function is to be used only within
[`createFormula`](https://pandora-isomemo.github.io/bmsc/reference/createFormula.md),
the validity of the input is not checked here but in
[`createFormula`](https://pandora-isomemo.github.io/bmsc/reference/createFormula.md).

## Usage

``` r
makePoly(vars, maxExponent, inverseExponent)
```

## Arguments

- vars:

  character: variable names

- maxExponent:

  integer: highest exponent

- inverseExponent:

  integer: highest inverse exponent

## Value

Character vector of `length(vars)` times `maxExponent`

## Examples

``` r
BMSC:::makePoly(vars = c("x1", "x2"), maxExponent = 3, inverseExponent = 2)
#> [1] "x1"       "x2"       "I(x1^2)"  "I(x2^2)"  "I(x1^3)"  "I(x2^3)"  "I(x1^-2)"
#> [8] "I(x2^-2)"
```
