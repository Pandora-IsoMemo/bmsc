#' Exclude rows with missing values
#' @description All rows with missing values on the variables from the model
#' formula are excluded. If all rows are excluded, an error occurs. If only some
#' of the rows are excluded, the number and percentage of excluded rows is
#' printed via a message. In addition, the corresponding positions from
#' the yUncertainty vector are excluded.
#' @param data data.frame
#' @param formula formula object
#' @param yUncertainty numeric: vector
#' @param imputeMissings boolean: impute missings
#' @param categorical character: categorical vars
#' @return A list with the elements "data" (data frame containing only the
#' relevant variables and complete rows) and "yUncertainty".
handleMissingData <- function(data, formula, yUncertainty, imputeMissings = FALSE, categorical = "") {
  
  all_vars <- all.vars(formula)
  numeric_vars <- all_vars[!(all_vars %in% categorical)]
  relevantData <- data[, names(data) %in% all_vars, drop = FALSE]
  
  if(imputeMissings & any(is.na(relevantData))){
    if(categorical != ""){
      for(i in categorical){
        if (inherits(relevantData[, i], "character")) {
          relevantData[, i] <- factor(relevantData[, i])
        }
      }
    }
    imputed_Data <- mice(relevantData, m=10, maxit = 50, seed = 500, printFlag = FALSE)
    completed <- complete(imputed_Data, "all")
    new_data <- relevantData
    if(length(numeric_vars) > 0){
      for (i in 1:length(numeric_vars)){
        new_data[, numeric_vars[i]] = rowMeans(sapply(1:length(completed), function(x) completed[[x]][,numeric_vars[i]]))
      }
    }
    if(any(categorical != "")){
      for (j in categorical){
        new_data[, j] <- apply(sapply(1:length(completed), function(x) completed[[x]][,j]), 1, getMode)
      }
    }
    relevantData <- new_data
    } else {
    completeRows <- complete.cases(relevantData)
    relevantData <- relevantData[completeRows, ]
    yUncertainty <- yUncertainty[completeRows]
  }
  
  
  if (nrow(relevantData) == 0) {
    stop("There are no rows without missing values in the data.")
  }
  
  nExcludedRows <- nrow(data) - nrow(relevantData)
  
  if (nExcludedRows > 0) {
    pctExcludedRows <- round(100 * nExcludedRows / nrow(data), 2)
    message(paste0(nExcludedRows, " rows (", pctExcludedRows,
                   "% of the data) were excluded due to missing values."))
  }
  
  return(list(data = relevantData, yUncertainty = yUncertainty))
}


getMode <- function(x){
  names(sort(-table(x)))[1]
}
