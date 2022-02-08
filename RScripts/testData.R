rm(list = ls(all.names = TRUE))
#install.packages("BMSC")
library(BMSC)

#simulate data
suppressWarnings(RNGversion("3.5.0"))
set.seed(72)
n <- 75
x1 <- rnorm(n, sd = 1)
x2 <- rnorm(n, sd = 1)
x3 <- rnorm(n, sd = 1)
x1Unc <- rnorm(n, sd = 0.1)
x2Unc <- rnorm(n, sd = 0.1)
x3Unc <- rnorm(n, sd = 0.1)
x4 <- rpois(n, lambda = 1)
x4Unc <- rnorm(n, sd = 0.1)

#regression formula
y <- 0.4 + 0.3 * x1 + 0.3 * x1 * x3 + 0.4 * x1 ^ 2 * x2 ^ 3 + 0.2 * x4  + rnorm(n, sd = 0.3)
# y <- round(y, 0)
y[y < 0] <- 0
y[y > 0] <- 1

yUncertainty <- rexp(n, 10) * 0.01
data <- data.frame(x1, x2, x3, x4 = as.character(x4), y, yUncertainty, x1Unc, x2Unc, x3Unc, x4Unc)

#estimate models
models <- constrSelEst(y ~ x1 + x2 + x3, mustInclude = c("x1", "x2"), maxExponent = 1,
                   #    categorical = "x4",
                       interactionDepth = 1, intercept = TRUE,
                       constraint_1 = FALSE, data = data,
                       type = "logistic",
                       ar1 = T,
                       #xUncertainty  = data[, c("x1Unc", "x2Unc", "x3Unc")],
                       #yUncertainty = yUncertainty,
                       maxNumTerms = 10,
                       scale = T,
                       chains = 4, iterations = 300)

fits <- getModelFits(models$models, y = data$y, newdata = data)

plotModelFit(models, thresholdSE = 1, markBestModel = TRUE, ic = "Rsq", fits = fits)

#get best model
bestModel <- getBestModel(models,  thresholdSE = 1, ic = "AIC")

print(bestModel$modelPlot)
bestModel$bestModel

###

library(xlsx)
data <- read.xlsx("~/Netzfreigaben/Git_TEX/result.xlsx", 1)

models <- constrSelEst(y ~ y1 + y2 + y3 + y4 + y5 + y6 + y7, maxExponent = 1,
                       interactionDepth = 1,
                       intercept = TRUE,
                       constraint_1 = FALSE, data = data,
                       type = "linear",
                       xUncertainty  = data[, c("y1_SE", "y2_SE", "y3_SE", "y4_SE", "y5_SE", "y6_SE", "y7_SE")],
                       yUncertainty = data[, "y_SE"],
                       maxNumTerms = 10,
                       scale = T,
                       chains = 4, iterations = 300)


#Another example (type: ?swiss for more info)

#if data is on very different scale either manual rescaling (mean = 0 and sd = 1) is necessary 
#OR: set scale = TRUE in constrSelEst() function
swissScaled <- as.data.frame(scale(swiss))

models <- constrSelEst(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,
                       maxExponent = 1, interactionDepth = 2, intercept = TRUE,
                       constraint_1 = FALSE, data = swissScaled, maxNumTerms = 10)

bestModel <- getBestModel(models,  thresholdSE = 1)
print(bestModel)

