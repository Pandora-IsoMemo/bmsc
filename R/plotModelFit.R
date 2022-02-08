#' Plot errors of all models
#' @description This plot is automatically produced with the execution of
#' \code{\link{getBestModel}}.
#' @param models List with models of class \code{\link{ConstrainedLinReg}}
#' @param fits Optional list with model fit measures from getModelFits() - function
#' @param ic information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"
#' @param thresholdSE numeric: Factor multiplied with standard error to obtain
#' ends of error bars
#' @param markBestModel boolean: highlight position of the best model in the model list
#' @param ylab character: y title
#' @param xlab character: x title
#' @param aSize numeric: axis label size
#' @param lSize numeric: axis title label size
#' @param tAngle numeric: x axis text angle
#' @export
plotModelFit <- function(models,
                         fits = NULL,
                         thresholdSE = 1,
                         markBestModel = TRUE,
                         ic = "Loo", 
                         ylab = NULL,
                         xlab = NULL,
                         aSize = 12,
                         lSize = 14,
                         tAngle = 30) {
  if(is.null(fits)){
    fits <- getModelFits(models)
  }
  if (markBestModel){
    bestSparse <- bestModel(models, fits[[ic]], thresholdSE, ic = ic)
  }
  
  modelNames <- prepModelNames(models)
  
  datPlot <- prepPlotData(fits = fits[[ic]],
                          thresholdSE = thresholdSE,
                          modelNames = modelNames,
                          ic = ic)
  
  colours <- if (!(markBestModel)) "black" else prepColorVec(bestSparse, length(models))
  
  plotModels(datPlot, colours, thresholdSE, ic = ic, 
             ylab, xlab, aSize, lSize, tAngle)
}


#' Extract model names from model objects
#' @description Extracts the model formulae from a list of model objects of
#' class \code{\link{ConstrainedLinReg}}. Elements that are superfluous for
#' reading (e.g., brackets) are removed.
#' @inheritParams plotModelFit
#' @export
prepModelNames <- function(models) {
  lapply(models, function(x) as.character(x@formula)[3]) %>%
    unlist %>% 
    gsub("I\\(|\\)", "", .)
}


#' Prepare data to plot model fit
#' @inheritParams plotModelFit
#' @param modelNames Names for the models in the same order as they appear in
#' \code{Loos}
#' @param ic information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"
#' @return A data.frame with the columns \code{Estimate} (Estimate of the
#' Looic), \code{SE}, \code{model}, \code{lower}, and \code{upper}
prepPlotData <- function(fits, modelNames, thresholdSE, ic = "Loo") {
  if(ic == "Loo"){
  datPlot <- lapply(fits, function(x) {
    res <- x$estimates["looic", c("Estimate", "SE")]
  }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame()
  datPlot$lower <- datPlot$Estimate - thresholdSE * datPlot$SE
  datPlot$upper <- datPlot$Estimate + thresholdSE * datPlot$SE
  }
  if(ic == "WAIC"){
    datPlot <- lapply(fits, function(x) {
      res <- x$estimates["waic", c("Estimate", "SE")]
    }) %>% 
      do.call(rbind, .) %>% 
      as.data.frame()
    datPlot$lower <- datPlot$Estimate - thresholdSE * datPlot$SE
    datPlot$upper <- datPlot$Estimate + thresholdSE * datPlot$SE
  }
  if(!(ic %in% c("Loo", "WAIC"))){
    datPlot <- data.frame(Estimate = fits, SE = 0, lower = NA, upper = NA)
  }
  datPlot$model <- modelNames
  datPlot
}


#' Prepare colour vector
#' @inheritParams plotModelFit
#' @param posBestModel numeric: position of best Model
#' @param length numeric: Length of colour vector
#' @return Vector of length \code{length}. It contains "black" expect for the
#' position provided in \code{posBestModel}, which is "chartreuse4" (green)
prepColorVec <- function(posBestModel, length) {
  colours <- rep("black", length)
  colours[posBestModel] <- "chartreuse4"
  colours
}


#' Plot model errors with errorbars
#' @param datPlot data.frame with prepared plot data
#' @param colours character: colour(s) for the points, bars and x-axis labels
#' @param ic information criterion: one of "Loo", "AIC", "WAIC", "BIC", "logLik"
#' @param ylab character: y title
#' @param xlab character: x title
#' @param aSize character: axis label size
#' @param lSize character: axis title label size
#' @param tAngle numeric: x axis text angle
#' @inheritParams plotModelFit
plotModels <- function(datPlot, colours, thresholdSE, ic, 
                       ylab = NULL, xlab = NULL, aSize = 12, lSize = 14,
                       tAngle = 30) {
  g <- ggplot(datPlot, aes_string(x = "model")) +
    geom_point(aes_string(y = "Estimate"), colour = colours) +
    geom_path(aes_string(y = "Estimate", x = 1:nrow(datPlot))) +
    theme(axis.text.x = element_text(angle = tAngle, hjust = 1, colour = colours)) +
    scale_x_discrete(labels = function(x) parse(text = x)) +
    theme(axis.text=element_text(size=aSize),
          axis.title=element_text(size=lSize,face="bold"))
  if(!is.null(xlab)){
    g <- g + xlab(xlab)
  }
  if(!is.null(ylab)){
    g <- g + xlab(ylab)
  }
  
  if(!is.na(datPlot$lower[1])){
    g <- g + geom_errorbar(aes_string(ymin = "lower", ymax = "upper"), colour = colours)
  }
  if(ic %in% c("Loo", "WAIC")){
    g <- g + labs(x = NULL,
         y = "",
         title = paste0("Fit of all models (Error bars show the ", ic ," +/- ",
                        thresholdSE,
                        "*SE)"))
  } else {
    g <- g + labs(x = NULL,
                  y = "",
                  title = paste0("Fit of all models: ", ic))
  }
  g
}
