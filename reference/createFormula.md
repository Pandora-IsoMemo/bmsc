# Create a formula with interactions and polynomials up to a desired order

Creates a formula with interactions and polynomials up to a desired
order. If the input `formula` already includes interactions, exponents
or other functions (e.g.,
[`sqrt`](https://rdrr.io/r/base/MathFun.html)), they are ignored.

## Usage

``` r
createFormula(
  formula,
  maxExponent = 1,
  inverseExponent = 1,
  interactionDepth = 1,
  intercept = TRUE,
  categorical = "",
  mustExclude = ""
)
```

## Arguments

- formula:

  formula object: formula object without exponents or interactions. If
  `formula` is not of class `formula`, it is turned into one.

- maxExponent:

  positive integer: highest exponent included in the formula. Default is
  1, e.g., only linear effects.

- inverseExponent:

  positive integer: highest inverse exponent included in the formula.
  Default is 1, e.g., only linear effects.

- interactionDepth:

  positive integer: maximum order of interaction. Default is 1, e.g.,
  only main effects (no interactions).

- intercept:

  logical: include intercept or not?

- categorical:

  character: categorical variables

- mustExclude:

  character: variables to exclude

## Value

A formula containing the original independent variables and their
polynomials and interactions.

## Examples

``` r
createFormula("y ~ x1 + x2", 2, 3)
#> y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1^-2) + I(x2^-2) + I(x1^-3) + 
#>     I(x2^-3)
#> <environment: 0x55a629908220>
createFormula(as.formula("y ~ x1 + x2"), interactionDepth = 2)
#> y ~ x1 + x2 + x1:x2
#> <environment: 0x55a62995ac78>

carFormula <- createFormula("mpg ~ cyl + disp + drat", 2, 3)
summary(lm(carFormula, mtcars))
#> 
#> Call:
#> lm(formula = carFormula, data = mtcars)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.4685 -1.4968 -0.1371  1.4470  3.8022 
#> 
#> Coefficients: (2 not defined because of singularities)
#>               Estimate Std. Error t value Pr(>|t|)  
#> (Intercept) -1.764e+03  1.483e+03  -1.189   0.2476  
#> cyl          2.973e+00  4.243e+00   0.701   0.4913  
#> disp         1.833e-01  9.901e-02   1.852   0.0782 .
#> drat         4.624e+02  4.012e+02   1.153   0.2620  
#> I(cyl^2)    -4.260e-01  3.627e-01  -1.174   0.2533  
#> I(disp^2)   -2.616e-04  1.243e-04  -2.105   0.0475 *
#> I(drat^2)   -3.628e+01  3.223e+01  -1.126   0.2731  
#> I(cyl^-2)           NA         NA      NA       NA  
#> I(disp^-2)   3.450e+05  2.743e+05   1.258   0.2223  
#> I(drat^-2)   1.204e+04  9.648e+03   1.248   0.2257  
#> I(cyl^-3)           NA         NA      NA       NA  
#> I(disp^-3)  -1.312e+07  1.510e+07  -0.869   0.3949  
#> I(drat^-3)  -1.754e+04  1.368e+04  -1.282   0.2139  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 2.178 on 21 degrees of freedom
#> Multiple R-squared:  0.9116, Adjusted R-squared:  0.8694 
#> F-statistic: 21.64 on 10 and 21 DF,  p-value: 7.297e-09
#> 
```
