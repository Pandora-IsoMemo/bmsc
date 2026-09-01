# Create formula with interactions and polynomials if all checks in [`createFormula`](https://pandora-isomemo.github.io/bmsc/reference/createFormula.md) have passed

Create formula with interactions and polynomials if all checks in
[`createFormula`](https://pandora-isomemo.github.io/bmsc/reference/createFormula.md)
have passed

## Usage

``` r
createFormulaInternal(
  formula,
  allVars,
  maxExponent,
  inverseExponent,
  interactionDepth,
  intercept,
  categorical,
  mustExclude
)
```

## Arguments

- formula:

  formula object

- allVars:

  object returned by [`all.vars`](https://rdrr.io/r/base/allnames.html)

- maxExponent:

  positive integer

- inverseExponent:

  positive integer

- interactionDepth:

  positive integer

- intercept:

  boolean

- categorical:

  character

- mustExclude:

  character
