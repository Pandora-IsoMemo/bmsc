# Extract variable name from polynomial expression

Extract variable name from polynomial expression

## Usage

``` r
extractVarname(x)
```

## Arguments

- x:

  Character: variables

## Examples

``` r
BMSC:::extractVarname(c("x1",
"I(x2^2)"))
#> [1] "x1" "x2"
```
