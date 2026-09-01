# S4 class for constrained linear regression models

Inherits from
[`stanfit`](https://mc-stan.org/rstan/reference/stanfit-class.html)

## Slots

- `formula`:

  model formula (class formula)

- `hasIntercept`:

  logical: Does the model formula include an intercept?

- `scaleCenter`:

  numeric: location scale of betas

- `scaleScale`:

  numeric: scale scale of betas

- `cLevel`:

  numeric: desired credible level

- `type`:

  character: regression type: "linear" or "logistic"

- `catVars`:

  character: names of categorical variables

- `designMatrix`:

  data.frame: design matrix

- `Loo`:

  numeric: Leave-one-out cross-validation

- `WAIC`:

  numeric: Widely applicable information criterion

- `df`:

  numeric: df

- `nagelkerke`:

  numeric: Nagelkerke's R^2

- `MallowsCP`:

  numeric: Mallows' Cp

- `AIC`:

  numeric: Akaike information criterion

- `AICc`:

  numeric: Corrected Akaike information criterion

- `BIC`:

  numeric: Bayesian information criterion

- `AUC`:

  numeric: Area under the curve

- `Rsq`:

  numeric: R^2

- `RsqAdj`:

  numeric: adjusted R^2

- `Bayes_Rsq`:

  numeric: Bayesian R^2

- `varNames`:

  character: variable names

- `model_name`:

  character: name of the Stan model

- `model_pars`:

  character: parameters of the Stan model

- `par_dims`:

  list: dimensions of the parameters

- `mode`:

  integer: mode of the sampler

- `sim`:

  list: simulation data

- `inits`:

  list: initial values for sampling

- `stan_args`:

  list: arguments passed to the Stan model

- `stanmodel`:

  list: compiled Stan model

- `date`:

  character: date of model creation

- `ar1`:

  logical: whether AR(1) structure is present

- `scaleYCenter`:

  numeric: location scale for Y variable

- `scaleYScale`:

  numeric: scale scale for Y variable
