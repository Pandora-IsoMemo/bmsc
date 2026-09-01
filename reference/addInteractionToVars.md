# Add interactions of a specific order to a vector of variables

Add interactions of a specific order to a vector of variables

## Usage

``` r
addInteractionToVars(order, vars)
```

## Arguments

- order:

  integer: order of the interaction

- vars:

  character: variables

## Value

Character vector

## Details

Interactions of variables with themselves (including polynomials of
themselves) are not included.

## Examples

``` r
BMSC:::addInteractionToVars(3, c("x1", "x2", "x3"))
#> [1] "x1:x2:x3"
```
