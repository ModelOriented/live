context("Creating simulated dataset of similar observations")

X1 <- tibble::tibble(x = 1:5, y = 6:10, z = 11:15, r = letters[1:5], s = letters[6:10]) # Case n = p
X2 <- X1[, c(1, 2, 5)] # Case n > p
X3 <- dplyr::bind_rows(X1, tibble::tibble(a = 16:20, b = letters[11:15])) # Case n < p

test_that("Any changes are made", {

})

test_that("Not more than one change is made per row", {

}) # Zależy od przypadku

test_that("Factors are treated correctly", {
  
})

test_that("Missing data are detected and treated properly", {

})


