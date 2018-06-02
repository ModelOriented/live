context("Creating simulated dataset of similar observations")

set.seed(1)
X <- as.data.frame(matrix(runif(5500), ncol = 11, nrow = 500))

count_diffs_in_rows <- function(table, row, explained_var) {
  col_no <- which(colnames(row) == explained_var)
  lapply(1:nrow(table), function(x) {
    row_of_table <- table[x, ]
    sum(row_of_table != row[, -col_no])
  }) %>%
    unlist() %>%
    sum()
}

test_that("Any changes are made", {
  expect_gt(
    count_diffs_in_rows((live::sample_locally2(data = X,
                                               explained_instance = X[3, ],
                                               explained_var = "V1",
                                               size = 50)$data), X[3, ], "V1"), 0)
})

test_that("Not too many changes are made", {
  expect_lte(
    count_diffs_in_rows((live::sample_locally2(data = X,
                                              explained_instance = X[3, ],
                                              explained_var = "V1",
                                              size = 50)$data), X[3, ], "V1"), 50)
  expect_lte(
    count_diffs_in_rows((live::sample_locally2(data = X,
                                               explained_instance = X[3, ],
                                               explained_var = "V1",
                                               size = 100)$data), X[3, ], "V1"), 100)
  expect_lte(
    count_diffs_in_rows((live::sample_locally2(data = X,
                                               explained_instance = X[3, ],
                                               explained_var = "V1",
                                               size = 20)$data), X[3, ], "V1"), 100)

})

test_that("Other sampling methods are okay", {
  expect_is(sample_locally2(X, X[3, ], "V1", 50, method = "lime"), "live_explorer")
  Xtmp <- X
  expect_is(normal_neighbourhood(Xtmp, Xtmp[3, ], 50, NULL,
                                 mu = rep(1, 11), Sigma = diag(1, 11)),
            "data.frame")
  Xtmp$cat <- sample(letters, nrow(Xtmp), replace = T)
  expect_is(normal_neighbourhood(Xtmp, Xtmp[3, ], 50, NULL,
                                 mu = rep(1, 11), Sigma = diag(1, 11)),
            "data.frame")
  expect_is(normal_neighbourhood(Xtmp, Xtmp[3, ], 2000, NULL,
                                 mu = rep(1, 11), Sigma = diag(1, 11)),
            "data.frame")
  expect_is(sample_locally2(X, X[3, ], "V1", 50, method = "normal",
                            NULL, mu = rep(1, 10), Sigma = diag(1, 10)),
            "live_explorer")
})

test_that("Missing data are detected warning is given", {
  Y <- X
  Y[1, 1] <- NA
  expect_warning(live:::check_for_na(Y, Y[1, ]))
  expect_warning(live:::check_for_na(X, X[4, ]), regexp = NA)
})

test_that("Checks are performed, variables are set constant", {
  expect_error(live:::check_conditions(X[10, FALSE], X[4, ], 50))
  expect_error(live:::check_conditions(X, X[4, ], -10))
  expect_error(live:::check_conditions(X[FALSE, 10, drop = F], X[4, ], 50))
  expect_error(live:::check_conditions(X[, -5, drop = F], X[4, -6], 50))
  expect_silent(sample_locally2(X, X[4, ], "V1", 50, fixed_variables = c("V5", "V6")))
})

test_that("Predictions are added", {
  local_dataset <- live::sample_locally2(data = X,
                                         explained_instance = X[3, ],
                                         explained_var = "V1",
                                         size = 50)
  local_dataset1 <- add_predictions2(local_dataset, "regr.lm", X)
  expect_output(add_predictions2(local_dataset, "regr.lm", X), regexp = NA)
  expect_is(local_dataset1$data[[local_dataset1$target]], "numeric")
  expect_is(local_dataset1$target, "character")
  local_dataset2 <- add_predictions2(local_dataset, lm(V1 ~., data = X))
  expect_output(add_predictions2(local_dataset, lm(V1 ~., data = X)), regexp = NA)
  expect_is(local_dataset2$data[[local_dataset2$target]], "numeric")
  expect_is(local_dataset2$target, "character")
  expect_error(add_predictions2(local_dataset, "regr.lm"))
})

