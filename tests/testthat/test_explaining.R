context("Fitting and plotting explanations")

set.seed(1)
X <- tibble::as_tibble(MASS::mvrnorm(50, rep(0, 10), diag(1, 10)))
local <- live::sample_locally(data = X,
                              explained_instance = X[3, ], 
                              explained_var = "V1",
                              size = 50)
local1 <- live::add_predictions(X, local, "regr.lm")
local_explained <- live::fit_explanation(local1, "regr.lm")

test_that("White box model is fitted correctly", {
  expect_is(local_explained, "WrappedModel")
  expect_is(mlr::getLearnerModel(local_explained), "lm")
})

test_that("Plots are created without problems", {
  expect_error(live::plot_explanation(local_explained, "waterfallplot", X[3, ]), regexp = NA)
  expect_error(live::plot_explanation(local_explained, "forestplot"), regexp = NA)
})

