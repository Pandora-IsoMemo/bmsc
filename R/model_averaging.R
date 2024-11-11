#' Compute model weights according to measure
#' @description Computes model weights according to measure
#' @param fits List of model fits (return of getModelFits()-function)
#' @param measure string with type of measure
#' 
#' @return Numeric vector of model weights
#' @export
get_model_weights <- function(fits, measure = "Loo"){
  
  if(measure == "Loo"){
    preWeights <- fits$Loo
    measures <- sapply(preWeights, function(x) x$estimates[rownames(x$estimates) == "looic"][1])
  }
  if(measure == "WAIC"){
    preWeights <- fits$WAIC
    measures <- sapply(preWeights, function(x) x$estimates[rownames(x$estimates) == "waic"][1])
  }
  
  if(measure == "AIC"){
    measures <- fits$AIC
  }
  if(measure == "AICc"){
    measures <- fits$AICc
  }
  if(measure == "logLik"){
    measures <- -2 * fits$logLik
  }
  if(measure == "BIC"){
    measures <- fits$BIC
  }
  
  if(exists("measures")){
    weights <- exp(-0.5 * (measures - min(measures)))
    return(weights/sum(weights))
  } else {
    return(NULL)
  }
}

#' Compute model average
#' @description Computes model weights according to measure
#' @param models_input Model list of class \code{\link{ConstrainedLinReg}}
#' @param weights Numeric vector of model weights 
#' 
#' @return Model that is model average due to model weigths
#' @export
get_avg_model <- function(models_input, weights){
  avg_model <- models_input[[length(models_input)]]
  all_vars <- attributes(avg_model)$varNames
  
  #beta
  beta_matrices <- lapply(1:length(models_input), function(x){
    b_matrix <- extract(models_input[[x]])$betaAll
    colnames(b_matrix) <- attributes(models_input[[x]])$varNames
    b_matrix_final <- matrix(0, ncol = length(all_vars), nrow = nrow(b_matrix))
    colnames(b_matrix_final) <- all_vars
    b_matrix_final[, colnames(b_matrix)] <- b_matrix
    b_matrix_final
  }) 
  x <- beta_matrices[[1]] * weights[1]
  tmp <- lapply(seq_along(beta_matrices)[-1], function(i){
    x <<- do.call("+", list(x, beta_matrices[[i]]* weights[i]) )
  })
  #sigma
  sigma_matrices <- lapply(1:length(models_input), function(x){
    s_matrix <- extract(models_input[[x]])$sigma
  }) 
  s <- sigma_matrices[[1]] * weights[1]
  tmp <- lapply(seq_along(sigma_matrices)[-1], function(i){
    s <<- do.call("+", list(s, sigma_matrices[[i]]* weights[i]) )
  })
  #ar
  if(avg_model@ar1){
    ar_matrices <- lapply(1:length(models_input), function(x){
      ar_matrix <- extract(models_input[[x]])$ar
    }) 
    a <- ar_matrix[[1]] * weights[1]
    tmp <- lapply(seq_along(ar_matrix)[-1], function(i){
      a <<- do.call("+", list(a, ar_matrix[[i]]* weights[i]) )
    })
  }
  
  nChains <- length(avg_model@stan_args)
  burnin <- avg_model@stan_args[[1]]$warmup
  iter <- avg_model@stan_args[[1]]$iter
  #manipulate model object (replace posterior samples)
  for (j in 1:nChains){
    for(i in 1:ncol(x)){
      avg_model@sim[[1]][[j]][[paste0("betaAll[",i,"]")]][(burnin+1):iter] <- 
        x[(1 + (j-1) * (iter-burnin)): ((j) * (iter-burnin)),i]
    }
    for(i in 1:ncol(s)){
      avg_model@sim[[1]][[j]][[paste0("sigma[",i,"]")]][(burnin+1):iter] <- 
        s[(1 + (j-1) * (iter-burnin)): ((j) * (iter-burnin)),i]
    }
    if(avg_model@ar1){
        avg_model@sim[[1]][[j]][["ar"]][(burnin+1):iter] <- 
          a[(1 + (j-1) * (iter-burnin)): ((j) * (iter-burnin))]
    }
  }
  return(avg_model)
}
