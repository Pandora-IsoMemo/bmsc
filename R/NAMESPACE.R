#' @import methods
#' @import rstantools
#' @aliases BMSC
#' @useDynLib BMSC, .registration = TRUE
#' @importFrom loo loo_compare extract_log_lik loo relative_eff waic
#' @importFrom mice mice complete
#' @importFrom Rcpp loadModule
#' @importFrom pROC roc
#' @importFrom dplyr %>% .data all_equal filter mutate_all n_distinct summarise rename group_by 
#' arrange desc
#' @importFrom ggplot2 aes_string element_text geom_errorbar geom_point ggplot
#' labs scale_x_discrete theme geom_path
#' @importFrom rstan extract stan_model sampling
#' @importFrom R.utils insert
#' @importFrom shiny incProgress
#' @importFrom stats as.formula complete.cases median model.matrix na.exclude napredict na.omit
#' quantile sd terms rnorm update var 
globalVariables(".")
