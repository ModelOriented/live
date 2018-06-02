context("Test for deprecated functions")

test_that("Any changes are made", {
  expect_gt(
    count_diffs_in_rows((live::sample_locally(data = X,
                                              explained_instance = X[3, ], 
                                              explained_var = "V1",
                                              size = 50)$data), X[3, ], "V1"), 0) 
})

test_that("Not too many changes are made", {
  expect_lte(
    count_diffs_in_rows((live::sample_locally(data = X,
                                              explained_instance = X[3, ], 
                                              explained_var = "V1",
                                              size = 50)$data), X[3, ], "V1"), 50)
  expect_lte(
    count_diffs_in_rows((live::sample_locally(data = X,
                                              explained_instance = X[3, ], 
                                              explained_var = "V1", 
                                              size = 100)$data), X[3, ], "V1"), 100)
  expect_lte(
    count_diffs_in_rows((live::sample_locally(data = X,
                                              explained_instance = X[3, ], 
                                              explained_var = "V1", 
                                              size = 20)$data), X[3, ], "V1"), 100)
  
})

test_that("Missing data are detected warning is given", {
  Y <- X
  Y[1, 1] <- NA
  expect_warning(check_for_na(Y, Y[1, ]))
  expect_warning(check_for_na(X, X[4, ]), regexp = NA)
})

test_that("Predictions are added", {
  local_dataset <- live::sample_locally(data = X,
                                        explained_instance = X[3, ], 
                                        explained_var = "V1",
                                        size = 50)
  local_dataset1 <- add_predictions(X, local_dataset, "regr.lm")
  expect_output(add_predictions(X, local_dataset, "regr.lm"), regexp = NA)
  expect_equal(length(local_dataset1), 3)
  expect_is(local_dataset1$data[[local_dataset1$target]], "numeric")
  expect_is(local_dataset1$target, "character")
  local_dataset2 <- add_predictions(X, local_dataset, lm(V1 ~., data = X))
  expect_output(add_predictions(X, local_dataset, lm(V1 ~., data = X)), regexp = NA)
  expect_equal(length(local_dataset2), 3)
  expect_is(local_dataset2$data[[local_dataset2$target]], "numeric")
  expect_is(local_dataset2$target, "character")
})

test_that("White box model is fitted correctly", {
  expect_error(fit_explanation2(sample_locally2(X, X[3, ], "V1", 50)))
  expect_is(local_explained_old, "WrappedModel")
  expect_is(mlr::getLearnerModel(local_explained_old), "lm")
  local1_old_tmp <- local1_old
  local1_old_tmp$data$V1 <- rep(1, 50)
  expect_error(fit_explanation(local1_old_tmp))
  expect_silent(fit_explanation(local1_old, response_family = "gaussian",
                                white_box = "regr.glm"))
  expect_error(fit_explanation(sample_locally(X, X[3, ], "V1", 50)))
})

test_that("Plots are created without problems", {
  expect_error(live::plot_explanation(local_explained_old, "waterfallplot", X_old[3, ]), regexp = NA)
  expect_error(live::plot_explanation(local_explained_old, "forestplot"), regexp = NA)
})

test_that("Other features", {
  expect_silent(sample_locally(X, X[4, ], "V1", 50, TRUE))
  Xd <- X
  Xd$V12 <- seq(from = lubridate::ymd("2015-01-01"), 
                to = lubridate::ymd("2020-01-01"), length.out = 500)
  expect_silent(sample_locally(Xd, Xd[4, ], "V1", 50))
  expect_silent(plot_explanation(local_explained2_old, explained_instance = X[3, ]))
  expect_is(plot_explanation2(local_explained, "waterfall"), "ggplot")
  expect_is(plot_explanation2(local_explained, "forest"), "ggplot")
  expect_silent(plot_explanation2(local_explained2))
  expect_is(plot_explanation2(local_explained3, "waterfall"), "ggplot")
  expect_silent(fit_explanation(local1_old, "regr.lm", selection = TRUE))
  expect_silent(plot_explanation(local2_explained_old, "waterfallplot", X2_old[3, ]))
})



