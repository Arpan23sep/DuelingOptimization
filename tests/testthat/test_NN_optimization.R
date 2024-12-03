library(testthat)
library(DuelingOptimization)

test_that("NN_optimization returns a list with valid weights and loss array", {

  #initializing weights
  initial_weights <- list(
    W1 = matrix(rnorm(20), 5, 4),
    b1 = rep(0, 4),
    W2 = matrix(rnorm(8), 4, 2),
    b2 = 0
  )

  #Define design matrix
  X <- matrix(rnorm(50), 10, 5)
  #define y
  y <- rnorm(10)

  #calling function
  result <- NN_optimization(initial_weights, X, y, eta = 0.01, gamma = 0.1, T = 100)

  #return output checking
  expect_type(result, "list")
  expect_named(result, c("weights", "loss_array"))
  expect_type(result$loss_array, "double")
  expect_gt(length(result$loss_array), 1)
})

test_that("NN_optimization returns correct values", {
  # Generate data
  set.seed(123)
  n_samples <- 20
  n_features <- 5
  n_hidden <- 3

  X <- matrix(rnorm(n_samples * n_features), n_samples, n_features)
  y <- rnorm(n_samples)

  # Initialize neural network weights
  initial_weights <- list(
    W1 = matrix(rnorm(n_features * n_hidden), n_features, n_hidden),
    b1 = rep(0, n_hidden),
    W2 = matrix(rnorm(n_hidden), n_hidden, 1),
    b2 = 0
  )

  # Set optimization parameters
  eta <- 0.01
  gamma <- 0.1
  T <- 100

  # Call NN_optimization
  result <- NN_optimization(initial_weights, X, y, eta, gamma, T)

  # Check if the result is a list
  expect_type(result, "list")

  # Check if result contains the expected components
  expect_named(result, c("weights", "loss_array"))

  # Check if the weights are structured as the input initial_weights
  expect_named(result$weights, c("W1", "b1", "W2", "b2"))

  # Check the dimensions of the optimized weights
  expect_equal(dim(result$weights$W1), dim(initial_weights$W1))
  expect_equal(length(result$weights$b1), length(initial_weights$b1))
  expect_equal(dim(result$weights$W2), dim(initial_weights$W2))
  expect_equal(length(result$weights$b2), length(initial_weights$b2))

  # Check if loss_array is numeric and has at least one value
  expect_type(result$loss_array, "double")
  expect_gt(length(result$loss_array), 1)

  # Check if the loss decreases over iterations
  expect_true(all(diff(result$loss_array) <= 0))
})
