test_that("test function bestModel", {
  set.seed(456)
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
  y <-
    0.4 + 0.3 * x1 + 0.3 * x1 * x3 + 0.4 * x1 ^ 2 * x2 ^ 3 + 0.2 * x4  + rnorm(n, sd = 0.3)
  # y <- round(y, 0)
  y[y < 0] <- 0
  y[y > 0] <- 1
  
  yUncertainty <- rexp(n, 10) * 0.01
  data <-
    data.frame(x1, x2, x3, x4 = as.character(x4), y, yUncertainty, x1Unc, x2Unc, x3Unc, x4Unc)

  #estimate models
  models <-
    constrSelEst(
      y ~ x1 + x2 + x3,
      mustInclude = c("x1", "x2"),
      maxExponent = 1,
      #    categorical = "x4",
      interactionDepth = 1,
      intercept = TRUE,
      constraint_1 = FALSE,
      data = data,
      type = "logistic",
      ar1 = T,
      #xUncertainty  = data[, c("x1Unc", "x2Unc", "x3Unc")],
      #yUncertainty = yUncertainty,
      maxNumTerms = 10,
      scale = T,
      chains = 2,
      iterations = 300
    )
  print(names(models$models))

  fits <- getModelFits(models$models, y = data$y, newdata = data, cores = getOption("mc.cores", 2))

  loo_names <- rownames(loo::loo_compare(fits[["Loo"]]))
  waic_names <- rownames(loo::loo_compare(fits[["WAIC"]]))

  print(names(models$models))
  print(loo_names)
  print(waic_names)

  testthat::expect_equal(
    bestModel(models$models, fits[["Loo"]], thresholdSE = 1, ic = "Loo"),
    1,
    info = paste(
      "model names:", paste(names(models$models), collapse = " | "),
      "\nloo_compare rownames:", paste(loo_names, collapse = " | ")
    )
  )
  testthat::expect_equal(
    bestModel(models$models, fits[["WAIC"]], thresholdSE = 1, ic = "WAIC"),
    1,
    info = paste(
      "model names:", paste(names(models$models), collapse = " | "),
      "\nwaic_compare rownames:", paste(waic_names, collapse = " | ")
    )
  )
  testthat::expect_equal(
    bestModel(models$models, fits[["RsqAdj"]], thresholdSE = 1, ic = "RsqAdj"),
    c(`y ~ x1 + x2 + x3` = 2L),
    info = paste(
      "model names:", paste(names(models$models), collapse = " | "),
      "\nRsqAdj rownames:", paste(rownames(fits[["RsqAdj"]]), collapse = " | ")
    )
  )

  for (ic in c("AIC",
               "AICc",
               "MallowsCP",
               "df",
               "nagelkerke",
               "BIC",
               "logLik",
               "Rsq",
               "AUC",
               "Bayes_Rsq")) {
    testRes <- bestModel(models$models,
                         fits[[ic]],
                         thresholdSE = 1,
                         ic = ic)
    # print(sprintf(
    #   "Testing %s: test result: %s, name: %s",
    #   ic,
    #   testRes,
    #   names(testRes)
    # ))
    testthat::expect_equal(testRes, c(`y ~ x1 + x2` = 1L))
  }
})
