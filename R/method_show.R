#' Print constraint estimation model
#' @description Prints the model formula and estimates as well as sigma with
#' the corresponding 95% credibility intervals.
#' @param object Model of class "MPIconstraintModel"
#' @export
setMethod("show",
          "ConstrainedLinReg",
          function(object) {
            cLevel <- object@cLevel
            betaMatrix <- getBetaMatrix(object, object@hasIntercept)
            sigmas <- extract(object)$sigma * object@scaleYScale
            arPar <- extract(object)$ar
            
            log_lik <- extract(object)$log_lik %>% rowSums %>% mean()
            rsq <- object@Rsq
            
            coefTable <- data.frame(
              Estimate = signif(colMeans(betaMatrix), 3),
              Median = signif(apply(betaMatrix, 2, median), 3),
              SD = signif(apply(betaMatrix, 2, sd), 3),
              Cred_Interval_ =
                paste0("[",
                       extractQuantile(betaMatrix,  (1 - cLevel) / 2),
                       ", ",
                       extractQuantile(betaMatrix, 1 - (1 - cLevel) / 2),
                       "]")
            )
            
            cat(displayFormula(object))
            print(coefTable)
            cat("\n")
            if(object@type == "linear"){
              cat(displaySigmas(sigmas, cLevel))
              cat("\n")
            }
            if(object@ar1 == TRUE){
              cat(displayAR1(arPar, cLevel))
              cat("\n")
            }
            
            if(!is.na(rsq)){
              cat(displayRsq(rsq))
            }
            cat("\n")
            cat(displayLogLik(log_lik))
            
            if(!is.na(object@AIC)){
              cat("\n")
              cat(displayAIC(object@AIC))
            }
            if(!is.na(object@BIC)){
              cat("\n")
              cat(displayBIC(object@BIC))
            }
            
            if(!is.na(object@WAIC)){
              cat("\n")
              cat(displayWAIC(object@WAIC))
            }
            if(!is.na(object@Loo)){
              cat("\n")
              cat(displayLoo(object@Loo))
            }
          }
)


#' Print constraint estimation model
#' @param x model object of class \code{\link{ConstrainedLinReg}}
#' @param cLevel numeric: desired credible level
#' @param fit single model fit (return of getModelFits()-function)
#' @param ... arguments passed from or to other methods
#' @export
print.ConstrainedLinReg <- function(x, cLevel = 0.95, fit = NULL,  ...) {
  x@cLevel = cLevel
  if (!is.null(fit)) {
  x@AIC = fit$AIC
  x@BIC = fit$BIC
  x@WAIC = fit$WAIC
  x@Loo = fit$Loo
  }
  methods::show(x, ...)
}


#' Extract beta matrix from \code{\link{ConstrainedLinReg}} model
#' @description Extracts matrix of beta estimates
#' @param model model object: Model of class \code{\link{ConstrainedLinReg}}
#' @param hasIntercept logical: Does the model formula include an intercept?
#' @return matrix of estimates
getBetaMatrix <- function(model, hasIntercept) {
  betaMatrix <- extract(model)$betaAll
  cnames <- attributes(model)$varNames
  if (hasIntercept) {
    betaMatrix[, 1] <-  model@scaleYCenter + 
      mean(model@scaleYScale) * (betaMatrix[, 1] - 
                             rowSums(sweep(betaMatrix[, - 1, drop = FALSE], 2,
                                   (model@scaleCenter / model@scaleScale), '*')))
    betaMatrix[, -1] <- sweep(betaMatrix[, - 1, drop = FALSE], 2,
                               (mean(model@scaleYScale) / model@scaleScale), '*')
  } else {
    betaMatrix <- sweep(betaMatrix, 2, (mean(model@scaleYScale) / model@scaleScale), '*')
  }
  colnames(betaMatrix) <- cnames
  
  return(betaMatrix)
}


extractQuantile <- function(betaMatrix, quant, digits = 3) {
  apply(betaMatrix,
        MARGIN = 2,
        function(x) signif(quantile(x, quant), digits))
}


displayFormula <- function(modelObj) {
  paste("Model formula:",
        Reduce(paste, deparse(modelObj@formula)),
        "\n\n")
}


displaySigmas <- function(sigmas, cLevel = 0.95) {
  paste0("Sigma: ",
         signif(mean(sigmas), 3),
         paste0(" (Cred_Interval_", cLevel, ": ["),
         signif(quantile(sigmas, (1 - cLevel) / 2), 3),
         ", ",
         signif(quantile(sigmas, 1 - (1 - cLevel) / 2), 3),
         "])")
}

displayRsq <- function(rsq) {
  paste("R-squared:",
        round(rsq, 3))
}


displayAR1 <-  function(arPar, cLevel = 0.95) {
  paste0("AR1_par: ",
         signif(mean(arPar), 3),
         paste0(" (Cred_Interval_", cLevel, ": ["),
         signif(quantile(arPar, (1 - cLevel) / 2), 3),
         ", ",
         signif(quantile(arPar, 1 - (1 - cLevel) / 2), 3),
         "])")
}

displayLogLik <- function(log_lik) {
  paste("Log-likelihood:",
        round(log_lik, 3))
}

displayAIC <- function(AIC) {
  paste("AIC:",
        round(AIC, 3))
}

displayWAIC <- function(WAIC) {
  paste("WAIC:",
        round(WAIC, 3))
}

displayLoo <- function(Loo) {
  paste("Loo (Leave-one-out error):",
        round(Loo, 3))
}


displayBIC <- function(BIC) {
  paste("BIC:",
        round(BIC, 3))
}

