#' S4 class for constrained linear regression models
#' @description Inherits from \code{\link[rstan]{stanfit}}
#' @slot formula model formula (class formula)
#' @slot hasIntercept logical: Does the model formula include an intercept?
#' @slot scaleCenter numeric: location scale of betas 
#' @slot scaleScale numeric: scale scale of betas 
#' @slot cLevel numeric: desired credible level
#' @slot type character: regression type: "linear" or "logistic" 
#' @slot catVars character: names of categorical variables
#' @slot designMatrix data.frame: design matrix
#' @slot Loo numeric: Leave-one-out cross-validation
#' @slot WAIC numeric: Widely applicable information criterion
#' @slot df numeric: df
#' @slot nagelkerke numeric: Nagelkerke's R^2
#' @slot MallowsCP numeric: Mallows' Cp
#' @slot AIC numeric: Akaike information criterion
#' @slot AICc numeric: Corrected Akaike information criterion
#' @slot BIC numeric: Bayesian information criterion
#' @slot AUC numeric: Area under the curve
#' @slot Rsq numeric: R^2
#' @slot RsqAdj numeric: adjusted R^2
#' @slot Bayes_Rsq numeric: Bayesian R^2
#' @slot varNames character: variable names
#' @slot model_name character: name of the Stan model
#' @slot model_pars character: parameters of the Stan model
#' @slot par_dims list: dimensions of the parameters
#' @slot mode integer: mode of the sampler
#' @slot sim list: simulation data
#' @slot inits list: initial values for sampling
#' @slot stan_args list: arguments passed to the Stan model
#' @slot stanmodel list: compiled Stan model
#' @slot date character: date of model creation
#' @slot ar1 logical: whether AR(1) structure is present
#' @slot scaleYCenter numeric: location scale for Y variable
#' @slot scaleYScale numeric: scale scale for Y variable
#' @export
ConstrainedLinReg <- setClass("ConstrainedLinReg",
                              slots = list(formula = "formula",
                                           hasIntercept = "logical",
                                           ar1 = "logical",
                                           scaleCenter = "numeric",
                                           scaleScale = "numeric",
                                           scaleYCenter = "numeric",
                                           scaleYScale = "numeric",
                                           cLevel = "numeric",
                                           type = "character",
                                           catVars = "character",
                                           designMatrix = "data.frame",
                                           Loo = "numeric",
                                           WAIC = "numeric",
                                           df = "numeric",
                                           nagelkerke = "numeric",
                                           MallowsCP = "numeric",
                                           AIC = "numeric",
                                           AICc = "numeric",
                                           BIC = "numeric",
                                           AUC = "numeric",
                                           Rsq = "numeric",
                                           RsqAdj = "numeric",
                                           Bayes_Rsq = "numeric",
                                           varNames = "character"),
                              contains = "stanfit")
