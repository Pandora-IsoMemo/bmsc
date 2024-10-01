#' Model selection algorithm for constrained estimation
#' @param formula formula object: formula object without exponents or
#' interactions. If \code{formula} is not of class \code{formula}, it is turned
#' into one.
#' @param mustInclude character vector: variables to include in any case; use ":" for interactions and "I(..)" for powers, e.g.: "I(x1^2):I(x2^3)".
#' @param mustExclude character vector: variables to exclude in any case; use ":" for interactions and "I(..)" for powers, e.g.: "I(x1^2):I(x2^3)".
#' @param categorical character vector: categorical variables
#' @param maxExponent positive integer: highest exponent included in the
#' formula. Default is 1, e.g., only linear effects.
#' @param inverseExponent positive integer: highest inverse exponent included in the
#' formula. Default is 1, e.g., only linear effects.
#' @param interactionDepth positive integer: maximum order of interaction.
#' Default is 1, e.g., only main effects (no interactions).
#' @param intercept logical: Should the intercept be included in the estimation or not?
#' @param constraint_1 logical: Should the all beta variables add up to 1?
#' @param ar1 logical: Should the an AR1 parameter be included for correlated errors?
#' @param data data.frame: dataset
#' @param yUncertainty numeric vector: optional, uncertainties in y variable
#' given in standard deviations
#' @param xUncertainty data.frame: optional, uncertainties in x variables. variable names must match with names in formula
#' @param xCatUncertainty data.frame: optional, uncertainties in categorical x variables. variable names must match with names in formula
#' @param type character: regression type: "linear" or "logistic" 
#' @param maxNumTerms positive integer: maximum number of variables to include
#' @param scale logical: should the variables be scaled to mean 0 and sd 1?
#' @param chains positive integer: number of chains for MCMC sampling
#' @param iterations positive integer: number of iterations per chain for MCMC sampling
#' @param burnin burnin
#' @param shiny logical: Used for shiny?
#' @param imputeMissings boolean: impute missings by pmm method in mice package?
#' @return A list of potential models
#' @examples
#' \dontrun{
#' suppressWarnings(RNGversion("3.5.0"))
#' set.seed(44)
#' n <- 80
#' x1 <- rnorm(n, sd = 1)
#' x2 <- rnorm(n, sd = 1)
#' x3 <- rnorm(n, sd = 1)
#' y <- 0.4 + 0.3 * x1 + 0.3 * x1 * x3 + 0.4 * x1 ^ 2 * x2 ^ 3 + rnorm(n, sd = 0.3)
#' yUncertainty <- rexp(n, 10) * 0.01
#' #optional (slow)
#' #xUncertainty <- data.frame(x3 = rep(0.1, n), x1 = rep(0.1, n), x2 = rep(1, n))
#' data <- data.frame(x1, x2, x3, y, yUncertainty)
#' models <- constrSelEst(y ~ x1 + x2 + x3, mustInclude = "x1", maxExponent = 3,
#'                        interactionDepth = 3, intercept = TRUE,
#'                        constraint_1 = TRUE, data = data,
#'                        yUncertainty = yUncertainty,
#'                        xUncertainty = NULL,
#'                        maxNumTerms = 10)
#' plotModelFit(models$models)
#' bestModel <- getBestModel(models$models, thresholdSE = 2)
#' print(bestModel)
#' }
#' @export
constrSelEst <- function(formula,
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
                         imputeMissings = FALSE) {
  if(constraint_1){
    scale <- FALSE
  }
  withoutMissings <- handleMissingData(data, formula, yUncertainty, imputeMissings, categorical)
  data <- withoutMissings$data
  if(categorical[1] != ""){
    data[, categorical] <- lapply(1:length(categorical),
                                  function(x) gsub(" ", "_", as.character(data[, categorical[x]]), fixed = TRUE))
  }
  yUncertainty <- withoutMissings$yUncertainty
  
  variableData <- selectModel(formula = formula,
                              data = data,
                              mustInclude = mustInclude,
                              mustExclude = mustExclude,
                              categorical = categorical,
                              maxExponent = maxExponent,
                              inverseExponent = inverseExponent,
                              interactionDepth = interactionDepth,
                              intercept = intercept,
                              constraint_1 = constraint_1,
                              ar1 = ar1,
                              yUncertainty = yUncertainty,
                              type = type,
                              scale = scale,
                              chains = chains,
                              burnin = burnin,
                              iterations = iterations,
                              shiny = shiny)
  
  models <- reAssessModels(formula = formula,
                           data = data,
                           variableData = variableData,
                           intercept = intercept,
                           constraint_1 = constraint_1,
                           ar1 = ar1,
                           categorical = categorical,
                           yUncertainty = yUncertainty,
                           xUncertainty = xUncertainty,
                           xCatUncertainty = xCatUncertainty,
                           mustInclude = mustInclude,
                           type = type,
                           maxNumTerms = maxNumTerms,
                           scale = scale,
                           chains = chains,
                           burnin = burnin,
                           iterations = iterations,
                           shiny = shiny)
  
  models <- models[!duplicated(names(models))]
  return(list(models = models, variableData = variableData))
}

selectModel <- function(formula,
                        data,
                        maxExponent,
                        inverseExponent,
                        interactionDepth,
                        intercept,
                        constraint_1,
                        ar1 = ar1,
                        yUncertainty,
                        type,
                        mustInclude,
                        mustExclude,
                        categorical,
                        scale,
                        chains,
                        burnin,
                        iterations,
                        shiny = FALSE) {
  
  if (any(yUncertainty < 0))
    stop("uncertainties in y must be larger or equal 0")
  data$yUncertainty <- yUncertainty
  regFormula <- createFormula(formula, maxExponent,inverseExponent, interactionDepth, intercept, categorical, mustExclude)
  mustIncludeFormula <- paste(paste(all.vars(regFormula)[1], "~",
                                    paste(mustInclude, collapse = "+")), "- 1")
  modelCompiled <-
   if (constraint_1) stanmodels$linRegHorseShoe else stanmodels$linRegHorseShoe
  #modelCompiled <- stan_model(file = "/nethome/mgross/Github_Projekte/MPI-constraint-estimation/inst/stan/linRegHorseShoe.stan")
  model <- estimateBayesianModel(data = data,
                                 regFormula = regFormula,
                                 mustIncludeFormula = mustIncludeFormula,
                                 intercept = intercept,
                                 modelCompiled = modelCompiled,
                                 ar1 = ar1,
                                 type = type,
                                 selectVars = TRUE,
                                 scale = scale,
                                 chains = chains,
                                 burnin = burnin,
                                 iterations = iterations,
                                 shiny = shiny)
  X <- model.matrix(as.formula(regFormula), data = data)
  X2 <- model.matrix(as.formula(mustIncludeFormula), data = data)
  
  if (NCOL(X2) > 0) {
    X <- X[, -which(duplicated(t(cbind(X, X2)), fromLast = TRUE)), drop = FALSE]
  }
    KSelect <- colMeans(abs(extract(model)$beta))
    KSelect <- order(-KSelect)[1:pmin(100, length(KSelect))]
  K <- unlist(lapply(KSelect,
                     function(x) {
                       mean(1 - 1 / (1 + (extract(model)$lambda[, x] ^ 2 *
                                            extract(model)$tau ^ 2 /
                                            rowMeans(extract(model)$sigma ^ 2) *
                                            NROW(data))))
                     }))
  if(intercept){
    variableData <- data.frame(variable = (c(colnames(X2), colnames(X)[-1][KSelect])),
                               K = c(rep(1, NCOL(X2)), round(K, 5)))
  } else {
  variableData <- data.frame(variable = c(colnames(X2), colnames(X)[KSelect]),
                             K = c(rep(1, NCOL(X2)), round(K, 5)))
  }
  variableData <- variableData[order(variableData$K, decreasing = TRUE), ]
  
  variableData$variableCat <- variableData$variable
  
  if(!is.null(categorical) & categorical[1] != ""){
    varParts <- strsplit(variableData$variable, "[:]")
    varParts2 <- lapply(varParts, function(z){
      res <- unique(sapply(1:length(categorical), function(k){
        assigner <- grep(categorical[k], z)
        zNew <- z
        if(length(assigner) > 0){
          zNew <- categorical[k]
        } else {
          zNew <- c("")
        }
        zNew
      }))
      res <- res[res != ""]
      if(length(res) == 0){
        res <- z
      }
      res
    })
    varParts2 <- lapply(varParts2, function(x) paste(x, collapse = ":"))
    variableData$variableCat <- unlist(varParts2)
    variableData <- variableData %>% group_by(.data$variableCat) %>% summarise(K = mean(K)) %>% 
      rename(variable = .data$variableCat) %>% arrange(desc(K))
  }
  
  return(na.omit(variableData))
}

reAssessModels <- function(formula,
                           data,
                           variableData,
                           intercept,
                           constraint_1,
                           ar1 = ar1,
                           categorical,
                           yUncertainty,
                           xUncertainty,
                           xCatUncertainty,
                           mustInclude,
                           maxNumTerms,
                           type,
                           scale,
                           chains,
                           burnin,
                           iterations,
                           Threshold = 0.01,
                           shiny = FALSE) {
  data$yUncertainty <- yUncertainty
  modelCompiled <- stanmodels$linReg
  # modelCompiled <- stan_model(file = "/nethome/mgross/Github_Projekte/MPI-constraint-estimation/inst/stan/linReg.stan")
  
  mustIncludeFormula <- paste(paste(all.vars(formula)[1], "~",
                                    paste(mustInclude, collapse = "+")), "- 1")
  Nterms <- max(max(1, length(mustInclude)),
                min(maxNumTerms,
                    length(na.omit(unlist(variableData[variableData$K > Threshold,
                                                       "variable"])))))
  regFormulas <- unique(lapply(1 : Nterms, function(x) {
    paste(all.vars(formula)[1], "~",
          paste(unlist(variableData[1 : x, "variable"]), collapse = "+"))
  }))
  
  rangeVars <- apply(model.matrix(as.formula(regFormulas[[length(regFormulas)]]),
                                  data = data)[, -1, drop = FALSE], 2, sd)
  
  if ((max(rangeVars) / min(rangeVars)) > 5000 & scale == FALSE) {
    warning("Predictor variables on very different scale. 
            Slow computation and unreliable results. 
            Please rescale your variables (or set scale = TRUE) or reduce max
            number of interactions and powers")
  }
  if (!intercept) {
    regFormulas <- unique(lapply(1:length(regFormulas),
                                 function(x) {paste(regFormulas[[x]], "- 1")}))
  }
  modelList <- lapply(regFormulas, function(formula) {
    estimateBayesianModel(data = data,
                          regFormula = formula,
                          intercept = intercept,
                          xUncertainty = xUncertainty,
                          xCatUncertainty = xCatUncertainty,
                          modelCompiled = modelCompiled,
                          mustIncludeFormula = mustIncludeFormula,
                          constraint_1 = constraint_1,
                          ar1 = ar1,
                          categorical = categorical,
                          type = type,
                          selectVars = FALSE,
                          scale = scale,
                          chains = chains,
                          burnin = burnin,
                          iterations = iterations,
                          shiny = shiny, 
                          nModels = length(regFormulas))
  })
  names(modelList) <- sapply(modelList, function(x) x@formula)
  modelList
}

estimateBayesianModel <- function(data,
                                  regFormula,
                                  mustIncludeFormula,
                                  intercept,
                                  xUncertainty = NULL,
                                  xCatUncertainty = NULL,
                                  modelCompiled,
                                  selectVars,
                                  categorical = "",
                                  type = "linear",
                                  constraint_1 = FALSE,
                                  ar1 = FALSE,
                                  mc = TRUE,
                                  shiny = FALSE,
                                  nModels = 1,
                                  scale,
                                  chains,
                                  burnin,
                                  iterations) {
  N <- NROW(data)
  X <- model.matrix(as.formula(regFormula), data = data)
  
  X2 <- model.matrix(as.formula(mustIncludeFormula), data = data)
  
  if (NCOL(X2) > 0) {
    X <- X[, -which(duplicated(t(cbind(X, X2)), fromLast = TRUE)), drop = FALSE]
  }
  
  if (!selectVars) {
    X <- cbind(X, X2)
  }
  
  betaConstr <- 0
  if(constraint_1){
    betaConstr <- 1
  }
  xunc <- 0
  xUncertaintyMatrix <- matrix(0, nrow = NROW(X), ncol = NCOL(X))
  if(!is.null(xUncertainty)){
    xUncertaintyMatrix <- getUncertaintyModelMatrix(data, xUncertainty, xCatUncertainty, regFormula, X)
    xunc <- 1
  }
  K <- NCOL(X)
  y <- data[, all.vars(as.formula(regFormula))[1]]
  yL <- y
  if(type == "linear"){
    logitR <- 0
    yL = rep(0, times = length(y))
  } else {
    logitR <- 1
    if(any(!(yL %in% c(0,1)))){
      stop("Y-Values that are not 0 or 1 found")
    }
  }
  
  yUncertainty <- data$yUncertainty
  K1 <- if (intercept) 1 else 0
  K2 <- NCOL(X2)
  
  scaleCenter <- rep(0, K)
  scaleScale <- rep(1, K)
  scaleYCenter <- 0
  scaleYScale <- 1
  
  if (selectVars | scale == TRUE) {
    if(type == "linear"){
      scaleYCenter <- mean(y)
      scaleYScale <- sd(y)
      y <- (y - scaleYCenter) / scaleYScale
      yUncertainty <- yUncertainty / scaleYScale
    }
    
    if (intercept) {
      scaledX <- scale(X[, -1, drop = F])
      X[, -1] <- scaledX
      X2[, -1] <- scale(X2[, -1, drop = F])
      if(!is.null(xUncertainty)){
        xUncertaintyMatrix[, -1] <- sweep(xUncertaintyMatrix[, -1, drop = F], 2, attr(scaledX, "scaled:scale"), '/')
      }
      scaleCenter <- attr(scaledX, "scaled:center")
      scaleScale <- attr(scaledX, "scaled:scale")
    } else {
      scaledX <- scale(X)
      X <- scaledX
      X2 <- scale(X2)
      if(!is.null(xUncertainty)){
        xUncertaintyMatrix <- sweep(xUncertaintyMatrix, 2, attr(scaledX, "scaled:scale"), '/')
      }
      scaleCenter <- attr(scaledX, "scaled:center")
      scaleScale <- attr(scaledX, "scaled:scale")
    }
    X2[is.na(X2)] <- 0
    X[is.na(X)] <- 0
    if(!is.null(xUncertainty)){
      xUncertaintyMatrix[is.na(xUncertaintyMatrix)] <- 0
    }
  }
  
  scaleScale[scaleScale == 0] <- 1
  
  cores <- getOption("mc.cores", if (mc) chains else 1)
  #if (!selectVars) cores <- 1
  # Compute stan model
  pars = c("betaAll", "sigma", "log_lik")
  if(!selectVars){
    pars = c(pars,"rsq", "sigmasq", "ar") 
  }
  if(selectVars){
    pars = c(pars,"lambda", "tau", "beta") 
  }
  ar1 <- as.numeric(ar1)
  varY = var(y)
  model <- suppressWarnings(rstan::sampling(modelCompiled,
                                            chains = chains,
                                            iter = iterations + burnin,
                                            warmup = burnin,
                                            cores = cores,
                                            pars = pars,
                                            verbose = FALSE,
                                            refresh = 0,
                                            control = list(adapt_delta = 0.8,
                                                           max_treedepth = 10)))
  
  if (shiny) {
    incProgress(1 / nModels / 2.5)
  }
  
  varNames <- colnames(X)
  regFormula <- as.formula(regFormula)
  
  if(NCOL(X2) > 0){
    varNames <- c(varNames, colnames(X2))
    for(i in 1:length(colnames(X2))){
      regFormula <- update(regFormula, paste("~ . +", colnames(X2)[i]))
    }
  }
  varNames <- unique(varNames)
  new("ConstrainedLinReg",
      model,
      formula = regFormula,
      ar1 = as.logical(ar1),
      hasIntercept = intercept,
      scaleCenter = scaleCenter,
      scaleScale = scaleScale,
      scaleYCenter = scaleYCenter,
      scaleYScale = scaleYScale,
      cLevel = 0.95,
      varNames = varNames,
      type = type,
      catVars = categorical,
      designMatrix = as.data.frame(X),
      AIC = as.numeric(NA),
      AICc = as.numeric(NA),
      BIC = as.numeric(NA),
      WAIC = as.numeric(NA),
      Loo = as.numeric(NA),
      Rsq = as.numeric(NA),
      RsqAdj = as.numeric(NA),
      Bayes_Rsq = as.numeric(NA),
      AUC = as.numeric(NA),
      df = as.numeric(NA),
      nagelkerke = as.numeric(NA),
      MallowsCP = as.numeric(NA))
}


getLoo <- function(stanfit) {
  log_lik1 <- extract_log_lik(stanfit, merge_chains = FALSE)
  rel_n_eff <- relative_eff(exp(log_lik1))
  loos <- loo(log_lik1, rel_n_eff, cores = getOption("mc.cores", 4))
  waics <- waic(log_lik1)
  return(list(loos = loos, waics = waics))
}

#' Get Best Model after Models Selection
#' 
#' @param models list of models fitted by \code{\link{constrSelEst}} function
#' @param thresholdSE numeric: How much standard errors in leave-one-out
#' prediction performance can the sparse model be worse than the best model
#' @param ic information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"
#' @return The best sparse model concerning leave-one-out performance within a
#' threshold
#' @export
getBestModel <- function(models, thresholdSE = 1, ic = "Loo") {
  fits <- getModelFits(models)
  bestSparse <- bestModel(models, fits[[ic]], thresholdSE, ic = ic)
  return(list(bestModel = models[[bestSparse]]))
}

#' Get Model Fits
#' 
#' @param y response variable
#' @param newdata data.frame containing all variables that appear in the model formula
#' @inheritParams getBestModel
#' 
#' @export
getModelFits <- function(models, y = NULL, newdata = NULL){
  loos <- suppressWarnings(lapply(models, getLoo))
  logLik <- lapply(models, function(x) extract(x)$log_lik)
  avgLiks <-  sapply(logLik, function(x) mean(rowSums(x)))
  nParam <- sapply(models, function(x) NCOL(extract(x)$betaAll))
  AICs <- 2 * nParam - 2 * avgLiks
  df <- sapply(models, function(x) NROW(x@designMatrix)) - nParam
  AICcs <- AICs + (2 * nParam ^ 2 + 2 * nParam) / (sapply(models, function(x) NROW(x@designMatrix)) - nParam - 1)
  BICs <- nParam * log(sapply(models, function(x) NROW(x@designMatrix))) - 2 * avgLiks
  WAICs <- lapply(loos, function(x) x$waics)
  loos <- lapply(loos, function(x) x$loos)
  RsqAdj <- sapply(models, function(x) mean(extract(x)$rsq))
  if(!is.null(y) & !is.null(newdata)){
  Bayes_Rsq <- sapply(models, function(x) bayes_R2_res(x, y, newdata))
  MallowsCP <- sapply(models, function(x) sum((y - predict(x, newdata = newdata))^2) / 
                        mean((y - predict(models[[length(models)]], newdata = newdata))^2) - NROW(x@designMatrix)) + 2 * (nParam + 1)
  } else {
    MallowsCP <- rep(NA, length(models))
    Bayes_Rsq <- rep(NA, length(models))
  }
  if(!is.null(y) & !is.null(newdata) & models[[1]]@type == "logistic"){
  nagelkerke <- sapply(models, function(x){
    p <- predict(x, newdata = newdata)
    L1 <-  sum(log(p) * y + log((1 - p)) * (1-y))
    L0 <-  sum(log(mean(y)) * y + log((1 - mean(y))) * (1-y))
    (1 - exp(L0 - L1) ^ (2 / NROW(x@designMatrix))) / (1 - exp(L0)^(2 / NROW(x@designMatrix)))
  })
  } else {
    nagelkerke <- rep(NA, length(models))
  }
  if(!is.null(y) & !is.null(newdata)){
    Rsq <- sapply(models, function(x) 1 - mean((y - predict(x, newdata = newdata))^2, na.rm = TRUE) / 
                    mean((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE))
  } else {
    Rsq <- rep(NA, length(models))
  }
  if(models[[1]]@type == "logistic"){
    AUC <- sapply(models, function(x) roc(y, predict(x, newdata = newdata))$auc)
  } else {
    AUC <- rep(NA, length(models))
  }
  return(list(Loo = loos,
              WAIC = WAICs, AIC = AICs, AICc = AICcs, BIC = BICs, MallowsCP = MallowsCP,
              logLik = avgLiks, RsqAdj = RsqAdj, Rsq = Rsq, Bayes_Rsq = Bayes_Rsq, AUC = AUC, df = df, nagelkerke = nagelkerke))
}

#' Best model
#' 
#' @param loos list of model fits
#' @inheritParams getBestModel
#'
#' @export
bestModel <- function(models, loos, thresholdSE, ic){
  if(ic == "Loo"){
    best <- which.max(lapply(loos, function(x) x$estimates["elpd_loo", "Estimate"]))
  }
  if(ic == "WAIC"){
    best <- which.max(lapply(loos, function(x) x$estimates["elpd_waic", "Estimate"]))
  }
  if(ic == "AIC"){
    best <- which.min(loos)
  }
  if(ic == "AICc"){
    best <- which.min(loos)
  }
  if(ic == "MallowsCP"){
    best <- which.min(loos)
  }
  if(ic == "df"){
    best <- which.max(loos)
  }
  if(ic == "nagelkerke"){
    best <- which.max(loos)
  }
  if(ic == "BIC"){
    best <- which.min(loos)
  }
  if(ic == "logLik"){
    best <- which.max(loos)
  }
  if(ic %in% c("Rsq", "RsqAdj", "AUC", "Bayes_Rsq")){
    best <- which.max(loos)
  }
  if(ic %in% c("Loo", "WAIC")){
    looC <- loo_compare(loos)
    comparison <- data.frame(looC, diffNumParam = unlist(unname(lapply(models, function(x) {
      NCOL(extract(models[[best]])$betaAll) - NCOL(extract(x)$betaAll)})))[match(rownames(looC), names(models))])
    # only sparse models
    comparison$elpd_diff [comparison$diffNumParam < 0] <- -Inf
    bestLoo <- which.max(comparison$elpd_diff + comparison$se_diff * thresholdSE)
    return(which(names(models) == rownames(comparison)[bestLoo]))
  } else {
    return(best)
  }
}

getUncertaintyModelMatrix <-
  function(data, xUncertainty, xCatUncertainty, regFormula, X) {
    dataUnc <- data
    indices <- na.omit(match(names(xUncertainty), names(data)))
    indicesCat <- na.omit(match(names(xCatUncertainty), names(data)))
    
    if (length(indices) > 0 | length(indicesCat) > 0) {
      modelMatrices <- lapply(1:1000, function(x) {
        
        if(length(indices) > 0){
          dataUnc[, match(names(xUncertainty), names(data))] <-
            data[, match(names(xUncertainty), names(data))] +
            do.call("cbind", lapply(1:NCOL(xUncertainty), function(x)
              rnorm(NROW(data), mean = 0, sd = xUncertainty[, x])))
        }
        
        if(length(indicesCat) > 0){
          dataUnc[, match(names(xCatUncertainty), names(data))] <-
            do.call("cbind", lapply(1:NCOL(xCatUncertainty), function(x)
              rnorm(NROW(data), mean = 0, sd = xCatUncertainty[, x])))
        }
        mUncertainty <-
          model.matrix(as.formula(regFormula), data = dataUnc)
      })
      xUncertaintyModelMatrix <-
        apply(simplify2array(modelMatrices), 1:2, sd)
    } else {
      xUncertaintyModelMatrix <-
        matrix(0, nrow = NROW(data), ncol = NCOL(X))
    }
    return(xUncertaintyModelMatrix)
  }

bayes_R2_res <- function(model, y, newdata) {
  ypred <- t(predict(model, newdata = newdata, samples = TRUE))
  e <- -1 * sweep(ypred, 2, y)
  var_ypred <- apply(ypred, 1, var)
  var_e <- apply(e, 1, var)
  mean(var_ypred / (var_ypred + var_e))
}
