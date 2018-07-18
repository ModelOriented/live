context("Test local permutation importance")

X_old
lm_model <- lm(V1 ~., data = X_old)

varimp <- local_permutation_importance(X_old[3, ], X_old, "V1", lm_model, 50)

testthat::test_that("S3 method are okay", {
  testthat::expect_output(print(varimp))
  testthat::expect_silent(plot(varimp))
})

testthat::test_that("Variable importance works", {
  testthat::expect_is(varimp, "local_permutation_importance")
  testthat::expect_is(varimp$residuals, "data.frame")
})