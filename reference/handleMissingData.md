# Exclude rows with missing values

All rows with missing values on the variables from the model formula are
excluded. If all rows are excluded, an error occurs. If only some of the
rows are excluded, the number and percentage of excluded rows is printed
via a message. In addition, the corresponding positions from the
yUncertainty vector are excluded.

## Usage

``` r
handleMissingData(
  data,
  formula,
  yUncertainty,
  imputeMissings = FALSE,
  categorical = ""
)
```

## Arguments

- data:

  data.frame

- formula:

  formula object

- yUncertainty:

  numeric: vector

- imputeMissings:

  boolean: impute missings

- categorical:

  character: categorical vars

## Value

A list with the elements "data" (data frame containing only the relevant
variables and complete rows) and "yUncertainty".
