# Model selection algorithm for constrained estimation

Model selection algorithm for constrained estimation

## Usage

``` r
constrSelEst(
  formula,
  data,
  mustInclude = "",
  mustExclude = "",
  categorical = "",
  maxExponent = 1,
  inverseExponent = 1,
  interactionDepth = 1,
  intercept = TRUE,
  constraint_1 = FALSE,
  ar1 = FALSE,
  yUncertainty = rep(0, NROW(data)),
  xUncertainty = NULL,
  xCatUncertainty = NULL,
  type = "linear",
  maxNumTerms = 10,
  scale = TRUE,
  chains = 4,
  burnin = 300,
  iterations = 500,
  shiny = FALSE,
  imputeMissings = FALSE
)
```

## Arguments

- formula:

  formula object: formula object without exponents or interactions. If
  `formula` is not of class `formula`, it is turned into one.

- data:

  data.frame: dataset

- mustInclude:

  character vector: variables to include in any case; use ":" for
  interactions and "I(..)" for powers, e.g.: "I(x1^2):I(x2^3)".

- mustExclude:

  character vector: variables to exclude in any case; use ":" for
  interactions and "I(..)" for powers, e.g.: "I(x1^2):I(x2^3)".

- categorical:

  character vector: categorical variables

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

  logical: Should the intercept be included in the estimation or not?

- constraint_1:

  logical: Should the all beta variables add up to 1?

- ar1:

  logical: Should the an AR1 parameter be included for correlated
  errors?

- yUncertainty:

  numeric vector: optional, uncertainties in y variable given in
  standard deviations

- xUncertainty:

  data.frame: optional, uncertainties in x variables. variable names
  must match with names in formula

- xCatUncertainty:

  data.frame: optional, uncertainties in categorical x variables.
  variable names must match with names in formula

- type:

  character: regression type: "linear" or "logistic"

- maxNumTerms:

  positive integer: maximum number of variables to include

- scale:

  logical: should the variables be scaled to mean 0 and sd 1?

- chains:

  positive integer: number of chains for MCMC sampling

- burnin:

  burnin

- iterations:

  positive integer: number of iterations per chain for MCMC sampling

- shiny:

  logical: Used for shiny?

- imputeMissings:

  boolean: impute missings by pmm method in mice package?

## Value

A list of potential models

## Examples

``` r
if (FALSE) { # \dontrun{
suppressWarnings(RNGversion("3.5.0"))
set.seed(44)
n <- 80
x1 <- rnorm(n, sd = 1)
x2 <- rnorm(n, sd = 1)
x3 <- rnorm(n, sd = 1)
y <- 0.4 + 0.3 * x1 + 0.3 * x1 * x3 + 0.4 * x1 ^ 2 * x2 ^ 3 + rnorm(n, sd = 0.3)
yUncertainty <- rexp(n, 10) * 0.01
#optional (slow)
#xUncertainty <- data.frame(x3 = rep(0.1, n), x1 = rep(0.1, n), x2 = rep(1, n))
data <- data.frame(x1, x2, x3, y, yUncertainty)
models <- constrSelEst(y ~ x1 + x2 + x3, mustInclude = "x1", maxExponent = 3,
                       interactionDepth = 3, intercept = TRUE,
                       constraint_1 = TRUE, data = data,
                       yUncertainty = yUncertainty,
                       xUncertainty = NULL,
                       maxNumTerms = 10)
plotModelFit(models$models)
bestModel <- getBestModel(models$models, thresholdSE = 2)
print(bestModel)
} # }
```
