context("Creating simulated dataset of similar observations")

set.seed(1)
X <- tibble::as_tibble(MASS::mvrnorm(50, rep(0, 20), diag(1, 20)))

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
