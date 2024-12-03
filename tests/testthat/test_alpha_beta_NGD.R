library(testthat)
library(DuelingOptimization)

test_that("alpha_beta_NGD outputs a valid solution", {
  # quadratic objective function
  f <- function(x) {
    sum((x - 3)^2)
  }

  # Set parameters for testing
  initial_point <- c(1, 1)
  alpha <- 1
  beta <- 1
  D <- 10
  tolerance <- 0.1

  # Call the alpha_beta_NGD function
  result <- alpha_beta_NGD(initial_point, alpha, beta, D, tolerance, f)

  # Check that the result is a numeric vector
  expect_type(result, "double")

  # Check that the result has the same length as the initial point
  expect_equal(length(result), length(initial_point))

  # Check that the function value at the result is close to the minimum
  final_value <- f(result)
  expect_true(final_value < f(initial_point))

  # Check that the result is close to the known minimizer
  expect_true(max(abs(result - 3)) < tolerance)
})

test_that("alpha_beta_NGD outputs exact solution", {
  # Define a function whose optimal value is 1,1
  f <- function(x) {
    sum((x - 1)^2)
  }

  # Set parameters randomly
  initial_point <- c(2, 2)
  alpha <- 1
  beta <- 1
  D <- 10
  tolerance <- 0.1

  # Call the alpha_beta_NGD function
  result <- alpha_beta_NGD(initial_point, alpha, beta, D, tolerance, f)

  # Check that the function value at the result is close to the minimum
  final_value <- f(result)
  expect_true(final_value < f(initial_point))

  # Check that the result is close to the known minimizer
  expect_true(max(abs(result - 1)) < tolerance)
})
