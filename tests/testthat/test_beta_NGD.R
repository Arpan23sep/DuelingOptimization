library(testthat)
library(DuelingOptimization)  # Replace with your package name if different

test_that("beta_NGD runs successfully and returns expected output structure", {
  # Define a simple quadratic function as the objective
  f <- function(x) sum((x - 2)^2)  # Minimum at x = c(2, 2, ..., 2)

  # Set up initial parameters
  initial_point <- c(4, 4)  # Start at (4, 4)
  eta <- 0.1  # Learning rate
  gamma <- 0.01  # Perturbation parameter
  T <- 100  # Number of iterations

  # Run beta_NGD
  result <- beta_NGD(initial_point, eta, gamma, T, f)

  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("optimum", "f_array"))
  expect_type(result$optimum, "double")
  expect_type(result$f_array, "double")

  # Check that f_array has T + 1 elements (initial value + iterations)
  expect_equal(length(result$f_array), T + 1)

  # Check that the last value of f_array is less than the first (optimization occurred)
  expect_lt(tail(result$f_array, 1), result$f_array[1])
})

test_that("beta_NGD throws error for invalid inputs", {
  f <- function(x) sum((x - 2)^2)  # Define the objective function

  # Invalid T
  expect_error(beta_NGD(c(4, 4), 0.1, 0.01, -10, f), "T must be a positive integer")

  # Invalid f
  expect_error(beta_NGD(c(4, 4), 0.1, 0.01, 100, "not_a_function"), "f must be a valid R function")

})

test_that("beta_NGD converges near the minimum", {
  f <- function(x) sum((x - 2)^2)  # Quadratic function with minimum at (2, 2, ..., 2)
  initial_point <- c(4, 4)  # Start at (4, 4)
  eta <- 0.1
  gamma <- 0.01
  T <- 100

  result <- beta_NGD(initial_point, eta, gamma, T, f)

  # Check that the optimum is close to the true minimum
  expect_true(all(abs(result$optimum - 2) < 0.1))  # Allow for small optimization error
})
