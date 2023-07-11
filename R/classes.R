#' S4 class for constrained linear regression models
#' @description Inherits from \code{\link[rstan]{stanfit}}
#' @slot formula model formula (class formula)
#' @slot hasIntercept logical: Does the model formula include an intercept?
#' @slot scaleCenter numeric: location scale of betas 
#' @slot scaleScale numeric: scale scale of betas 
#' @slot cLevel numeric: desired credible level
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
