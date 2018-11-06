library(testthat)
library(live)
library(mlr)
library(lubridate)
library(DALEX)
library(data.table)

set.seed(1)
X <- as.data.frame(matrix(runif(5500), ncol = 11, nrow = 500))
X2 <- X
X2$V1 <- as.factor(as.character(X2$V1 > 0.5))
local <- sample_locally(data = X,
                         explained_instance = X[3, ],
                         explained_var = "V1",
                         size = 50)
local1 <- add_predictions(local, "regr.svm", X)
local_explained <- fit_explanation(local1, "regr.lm")
local_explained2 <- fit_explanation(local1, "regr.svm", kernel = identity_kernel)

X_old <- X
X2_old <- X_old
X2_old$V1 <- as.factor(as.character(X2_old$V1 > 0.5))

local2 <- sample_locally(data = X2, explained_instance = X2[3, ],
                          explained_var = "V1", size = 500)
local3 <- add_predictions(local2, "classif.svm", X2)
local_explained3 <- fit_explanation(local3, "classif.logreg", predict_type = "prob")

X$V3 <- as.factor(as.character(round(X$V3)))
local4 <- sample_locally(data = X,
                          explained_instance = X[3, ],
                          explained_var = "V1",
                          size = 50)
local4 <- add_predictions(local4, "regr.svm", X)

X_factors <- X
X_factors$V4 <- as.factor(as.character(round(X_factors$V4)))

count_diffs_in_rows <- function(table, row, explained_var) {
  col_no <- which(colnames(row) == explained_var)
  lapply(1:nrow(table), function(x) {
    row_of_table <- table[x, ]
    sum(row_of_table != row[, -col_no])
  }) %>%
    unlist() %>%
    sum()
}

test_check("live")

