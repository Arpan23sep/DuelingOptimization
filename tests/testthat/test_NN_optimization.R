library(testthat)
library(DuelingOptimization)

test_that("NN_optimization returns a list with valid weights and loss array", {
  initial_weights <- list(
    W1 = matrix(rnorm(20), 5, 4),
    b1 = rep(0, 4),
    W2 = matrix(rnorm(8), 4, 2),
    b2 = 0
  )
  X <- matrix(rnorm(50), 10, 5)
  y <- rnorm(10)

  result <- NN_optimization(initial_weights, X, y, eta = 0.01, gamma = 0.1, T = 100)

  expect_type(result, "list")
  expect_named(result, c("weights", "loss_array"))
  expect_type(result$loss_array, "double")
  expect_gt(length(result$loss_array), 1)
})
