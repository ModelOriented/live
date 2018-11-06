context("Fitting and plotting explanations")

test_that("White box model is fitted correctly", {
  expect_is(local_explained, "live_explainer")
  expect_silent(live:::create_task("classif.logreg", X2, "V1"))
  expect_is(mlr::getLearnerModel(local_explained$model), "lm")
  expect_error(fit_explanation(sample_locally(X, X[3, ], "V1", 50)))
  local1_tmp <- local1
  local1_tmp$data$V1 <- rep(1, 50)
  expect_error(fit_explanation(local1_tmp))
  expect_silent(fit_explanation(local1, "regr.glm",
                                 response_family = "gaussian"))
})

test_that("Kernels are okay", {
  expect_equal(identity_kernel(1:10, 1:10), 1)
  expect_equal(gaussian_kernel(1:10, 1:10), 1)
  expect_equal(euclidean_kernel(1:10, 1:10), 1)
  expect_true(identity_kernel(1:10, 1:10) == 1)
  expect_true(gaussian_kernel(1:10, runif(10)) != 1)
  expect_true(euclidean_kernel(1:10, runif(10)) != 1)
})

test_that("Plots are created without problems", {
  expect_output(plot(local_explained, type = "waterfall"), regexp = NA)
  expect_output(plot(local_explained, type = "forest"), regexp = NA)
  expect_output(plot(local_explained2), regexp = NA)
  expect_is(plot(local_explained3, type = "waterfall"), "ggplot")
})


test_that("Generics work", {
  expect_output(print(local1))
  expect_output(print(sample_locally(X, X[3, ], "V1", 50)))
  expect_output(print(local_explained))
  expect_output(print(fit_explanation(local1, selection = T)))
  expect_output(print(fit_explanation(local4, selection = T)))
  expect_output(print(fit_explanation(local1, kernel = identity_kernel)))
  expect_output(print(fit_explanation(local1, "regr.svm")))
  expect_output(print(add_predictions(local3, e1071::svm(V1~.,data = X2))))
})

test_that("Shiny app is fine", {
  expect_silent(live_shiny(X, e1071::svm(V1 ~., data = X)))
})

test_that("Variable selection", {
  expect_silent(fit_explanation(local1, selection = TRUE))
  expect_silent(fit_explanation(local4, selection = TRUE))
  expect_is(select_variables(X_factors, "V1", "gaussian"), "character")
})

test_that("Standardize option works", {
  set.seed(17)
  Xs <- as.data.frame(matrix(runif(5500), ncol = 11, nrow = 500))
  locals <- sample_locally(Xs, Xs[4, ], "V1", 50)
  locals <- add_predictions(locals, "regr.lm", Xs)
  locals_expl <- fit_explanation(locals, standardize = TRUE)
  d1 <- locals$data[, 1:10]
  d2 <- locals_expl$data[, 1:10]
  expect_equal(as.data.frame(sapply(d1, function(x) scale(x, scale = F))), d2[, 1:10])
  expect_silent(fit_explanation(local4, standardize = TRUE))
})
