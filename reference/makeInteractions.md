# Add all interactions up to a desired order

Add all interactions up to a desired order

## Usage

``` r
makeInteractions(vars, interactionDepth)
```

## Arguments

- vars:

  character: variable names (potentially including polynomial
  expressions)

- interactionDepth:

  integer: highest interaction order

## Value

Character vector

## Details

Interactions of variables with themselves (including polynomials of
themselves) are not included.

## Examples

``` r
BMSC:::makeInteractions(vars = c("x1", "x2",
"I(x1^2)", "I(x2^2)"), interactionDepth = 3)
#> [1] "x1"              "x2"              "I(x1^2)"         "I(x2^2)"        
#> [5] "x1:x2"           "I(x2^2):x1"      "I(x1^2):x2"      "I(x1^2):I(x2^2)"
```
