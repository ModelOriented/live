context("Fitting and plotting explanations")

set.seed(1)
X <- as.data.frame(matrix(runif(5500), ncol = 11, nrow = 500))
X2 <- X
X2$V1 <- as.factor(as.character(X2$V1 > 0.5))
local <- sample_locally2(data = X,
                         explained_instance = X[3, ],
                         explained_var = "V1",
                         size = 50)
local1 <- add_predictions2(local, "regr.svm", X)
local_explained <- fit_explanation2(local1, "regr.lm")
local_explained2 <- fit_explanation2(local1, "regr.ctree", kernel = identity_kernel)

local2 <- sample_locally2(data = X2, explained_instance = X2[3, ],
                         explained_var = "V1", size = 5000)
local3 <- add_predictions2(local2, "classif.svm", X2)
local_explained3 <- fit_explanation2(local3, "classif.logreg", predict_type = "prob")

test_that("White box model is fitted correctly", {
  expect_is(local_explained, "live_explainer")
  expect_is(mlr::getLearnerModel(local_explained$model), "lm")
})

test_that("Plots are created without problems", {
  expect_output(plot(local_explained, type = "waterfall"), regexp = NA)
  expect_output(plot(local_explained, type = "forest"), regexp = NA)
  expect_output(plot(local_explained2), regexp = NA)
  expect_is(plot(local_explained3, type = "waterfall"), "ggplot")
})

