context("Test shortcut for DALEX explainers")

mlm <- lm(quality ~., data = live::wine)
expl <- DALEX::explain(mlm, live::wine, live::wine$quality)

testthat::test_that("DALEX integration is okay", {
  testthat::expect_is(
    local_approximation(expl, live::wine[5, ], "quality", 500),
    "live_explainer"
    )
})
