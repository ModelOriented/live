context("Test local permutation importance")

X_old_long_names <- X_old
colnames(X_old_long_names) <- paste0(colnames(X_old), "aaaaaaaa")
lm_model <- lm(V1 ~., data = X_old)
lm_model2 <- lm(V1aaaaaaaa ~., data = X_old_long_names)

varimp <- local_permutation_importance(X_old[3, ], X_old, "V1", lm_model, 50)
varimp2 <- local_permutation_importance(X_old_long_names[3, ], 
                                        X_old_long_names, "V1aaaaaaaa",
                                        lm_model2, 50)

testthat::test_that("S3 method are okay", {
  testthat::expect_output(print(varimp))
  testthat::expect_output(print(varimp2))
  testthat::expect_silent(plot(varimp))
})

testthat::test_that("Variable importance works", {
  testthat::expect_is(varimp, "local_permutation_importance")
  testthat::expect_is(varimp$residuals, "data.frame")
})

