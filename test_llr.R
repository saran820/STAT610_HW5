context("Check local linear regression function")
source("llr_functions.R")

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)

## test other data
## library(reshape2)

## data(french_fries)
## french_fries = french_fries[complete.cases(french_fries),]
## x = french_fries$potato
## y = french_fries$buttery
## z = seq(0, 15, length.out = 100)
## n <- length(x)
## fits = llr(z = z, x = x, y = y, omega = 2)
## plot(z, fits)


test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})


test_that("make_weight_matrix works on simple cases", {
  ## check that the output is a diagonal matrix, that all the elements are positive, that the weights are correct in simple cases where you know what the output shuold be
  z_i = z[1]
  wm = make_weight_matrix(z_i, x, omega = 1)
  
  # Check if it's a square matrix of size 'n'
  expect_equal(dim(wm), c(n, n))
  
  # Check if it's diagonal
  expect_equal(sum(diag(wm)), sum(wm))
  
  # Check if all elements are positive
  expect_true(all(diag(wm) >= 0))
})

test_that("make_predictor_matrix works on simple cases", {
  ## write tests to check that the dimensions are correct, the first column is all 1's, etc.
  pm = make_predictor_matrix(x)
  
  # Check if dimensions are correct
  expect_equal(dim(pm), c(n, 2))
  
  # Check if first column is all 1's
  expect_equal(sum(pm[, 1]), n)
  
  # Check if second column matches the input vector 'x'
  expect_equal(pm[, 2], x)
})