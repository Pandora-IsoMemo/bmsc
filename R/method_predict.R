#' Compute predictions from constraint estimation model
#' @description Computes prediction from model of class
#' \code{\link{ConstrainedLinReg}} and a data.frame.
#' @param object Model of class \code{\link{ConstrainedLinReg}}
#' @param newdata data.frame containing all variables that appear in the model
#' formula
#' @param samples boolean return samples of predictions?
#' 
#' @return Numeric vector of predictions. For observations with missing
#' values on the explanatory variables, a prediction of \code{NA} is returned.
#' @export
setMethod("predict",
          "ConstrainedLinReg",
          function(object, newdata, samples = FALSE) {
            newdataPrep <- prepDatForPredict(object@formula, newdata, object@catVars)
            X <- model.matrix(object = object@formula, data = newdataPrep)
            # if(object@hasIntercept){
            #   X <- sweep(X[, -1, drop = F], 2, object@scaleCenter, '-')
            #   X <- sweep(X, 2, object@scaleScale, '/')
            # } else {
            #   X <- sweep(X, 2, object@scaleCenter, '-')
            #   X <- sweep(X, 2, object@scaleScale, '/')
            # }
            beta <- getBetaMatrix(object, object@hasIntercept)
            predX <- sapply(1:nrow(beta), function(x) as.vector(X %*% beta[x,]))
            if(object@type == "logistic"){
              predX <- exp(predX)  / (1 + exp(predX))
            }
            if(samples){
              pred <- predX
            } else {
              pred <- rowMeans(predX)
              pred <- napredict(attributes(newdataPrep)$na.action, pred)
            }
            return(pred)
          }
)


#' Exclude rows with missing data on predictor variables
#' @description Rows with missing values on predictor variables are excluded.
#' An unused column for the dependent variable is added to avoid errors.
#' @details A column of ones for the dependent variable is added. Otherwise
#' \code{\link[stats]{model.matrix}} tries to take it from the formula's
#' environment, which is the original data. This usually results in an error due
#' to unequal variable length. This column is however not used.
#' @param formula Model formula
#' @param newdata data.frame containing all variables that appear in the model
#' @return Object of class \code{\link[stats]{na.exclude}}
prepDatForPredict <- function(formula, newdata, catVars) {
  if(catVars[1] != ""){
    newdata[, catVars] <- lapply(1:length(catVars), function(x) as.character(newdata[, catVars[x]]))
  }
  predictors <- all.vars(formula, functions = FALSE)[-1]
  datRes <- na.exclude(newdata[, predictors, drop = FALSE])
  
  # Add column of ones for dependent variable.
  datRes[[all.vars(formula, functions = FALSE)[1]]] <- rep(1, nrow(datRes))
  
  return(datRes)
}
