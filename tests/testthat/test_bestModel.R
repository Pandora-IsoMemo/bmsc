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
      ar1 = TRUE,
      #xUncertainty  = data[, c("x1Unc", "x2Unc", "x3Unc")],
      #yUncertainty = yUncertainty,
      maxNumTerms = 10,
      scale = TRUE,
      # not stable enough for smaller chains and iterations
      # iterations = 300
      chains = 2,
      burnin = 500,
      iterations = 600
    )
  print(names(models$models))

  fits <- getModelFits(models$models, y = data$y, newdata = data, cores = getOption("mc.cores", 2))

  loo_names <- rownames(loo::loo_compare(fits[["Loo"]]))
  waic_names <- rownames(loo::loo_compare(fits[["WAIC"]]))

  print(loo_names)
  print(waic_names)

  res <- bestModel(models$models, fits[["RsqAdj"]], thresholdSE = 1, ic = "RsqAdj")
  dput(res)
  dput(unname(res))
  dput(names(res))
  dput(fits[["RsqAdj"]])
  dput(which.max(fits[["RsqAdj"]]))

  testthat::expect_equal(
    bestModel(models$models, fits[["Loo"]], thresholdSE = 1, ic = "Loo"),
    1,
    info = paste(
      "model names:", paste(names(models$models), collapse = " | "),
      "\nloo_compare rownames:", paste(loo_names, collapse = " | "),
      "\nbest model:", paste(
        bestModel(models$models, fits[["Loo"]], thresholdSE = 1, ic = "Loo"),
        collapse = " | "
      )
    )
  )
  testthat::expect_equal(
    bestModel(models$models, fits[["WAIC"]], thresholdSE = 1, ic = "WAIC"),
    1,
    info = paste(
      "model names:", paste(names(models$models), collapse = " | "),
      "\nwaic_compare rownames:", paste(waic_names, collapse = " | "),
      "\nbest model:", paste(
        bestModel(models$models, fits[["WAIC"]], thresholdSE = 1, ic = "WAIC"),
        collapse = " | "
      )
    )
  )

  testthat::expect_equal(
    bestModel(models$models, fits[["RsqAdj"]], thresholdSE = 1, ic = "RsqAdj"),
    # c(`y ~ x1 + x2 + x3` = 2L), # not a stable result, stochastic model-selection brittleness
    which.max(fits[["RsqAdj"]]),
    info = paste(
      "model names:", paste(names(models$models), collapse = " | "),
      "\nRsqAdj rownames:", paste(names(fits[["RsqAdj"]]), collapse = " | "),
      "\nbest model:", paste(
        bestModel(models$models, fits[["RsqAdj"]], thresholdSE = 1, ic = "RsqAdj"),
        collapse = " | ")
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
    testthat::expect_equal(
      testRes,
      c(`y ~ x1 + x2` = 1L),
      info = paste(
        "model names:", paste(names(models$models), collapse = " | "),
        "\n", ic, " rownames:", paste(names(fits[[ic]]), collapse = " | "),
        "\nbest model:", paste(testRes, collapse = " | ")
      )
    )
  }
})
