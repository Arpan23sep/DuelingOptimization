library(testthat)
library(DuelingOptimization)

test_that("beta_NGD_optimum runs successfully and returns expected output structure", {
  # quadratic function as the objective
  f <- function(x) sum((x - 2)^2)  # Minimum at x = c(2, 2, ..., 2)

  # Set up initial parameters
  initial_point <- c(4, 4)
  D <- 5  # Diameter of the search space
  eigen_max <- 2  # Maximum eigenvalue of the Hessian
  epsilon <- 0.1  # Desired optimization accuracy

  # Run beta_NGD_optimum
  result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon, f)

  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("optimum", "f_array"))
  expect_type(result$optimum, "double")
  expect_type(result$f_array, "double")

  # Check that f_array contains at least one value
  expect_gt(length(result$f_array), 0)

  # Check that the optimum has the same dimension as the initial_point
  expect_equal(length(result$optimum), length(initial_point))
})

test_that("beta_NGD_optimum throws errors for invalid inputs", {
  f <- function(x) sum((x - 2)^2)

  # Invalid initial_point
  expect_error(beta_NGD_optimum(NULL, 5, 2, 0.1, f), "initial_point must be a non-empty numeric vector")

  # Invalid D
  expect_error(beta_NGD_optimum(c(4, 4), -5, 2, 0.1, f), "D must be a positive numeric value")

  # Invalid eigen_max
  expect_error(beta_NGD_optimum(c(4, 4), 5, -2, 0.1, f), "eigen_max must be a positive numeric value")

  # Invalid epsilon
  expect_error(beta_NGD_optimum(c(4, 4), 5, 2, -0.1, f), "epsilon must be a positive numeric value")

  # Invalid f
  expect_error(beta_NGD_optimum(c(4, 4), 5, 2, 0.1, "not_a_function"), "f must be a valid R function")
})

test_that("beta_NGD_optimum converges to a point near the minimum", {
  f <- function(x) sum((x - 2)^2)  # Quadratic function with minimum at (2, 2, ..., 2)
  initial_point <- c(4, 4)
  D <- 20
  eigen_max <- 2
  epsilon <- 0.1

  #calling the function
  result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon, f)

  # Check that the optimum is close to the true minimum
  expect_true(all(abs(result$optimum - 2) < 0.1))
})


test_that("beta_NGD_optimum runs successfully and returns expected output structure", {

  #no of samples
  n_samples <- 100
  #beta to generate data
  beta <- c(4,3,2,3)
  I <- rep(1, n_samples)                        # Intercept term
  X1 <- runif(n_samples, 0, 1)                  # Feature 1
  X2 <- runif(n_samples, 0, 0.5)                # Feature 2
  X3 <- runif(n_samples, 0, 0.8)                # Feature 3
  X <- cbind(I, X1, X2, X3)

  #generating y
  y <- X %*% beta

  # Define the objective function
  f <- function(beta) sum((y - X %*% beta)^2)
  initial_point <- c(5, 2, 3, 2)

  D <- 5  # Diameter of the search space
  Hessian <- 2 * t(X) %*% X       #hessian for regression case
  eigenvalues <- eigen(Hessian)$values
  eigen_max <- max(eigenvalues)
  epsilon <- 0.1  # Desired optimization accuracy

  # Run beta_NGD_optimum
  result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon, f)

  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("optimum", "f_array"))
  expect_type(result$optimum, "double")
  expect_type(result$f_array, "double")

  # Check that f_array contains at least one value
  expect_gt(length(result$f_array), 0)

  # Check that the optimum has the same dimension as the initial_point
  expect_equal(length(result$optimum), length(initial_point))
})

test_that("beta_NGD_optimum throws errors for invalid inputs", {
  #no of samples
  n_samples <- 100
  #beta to generate data
  beta <- c(4,3,2,3)
  I <- rep(1, n_samples)                        # Intercept term
  X1 <- runif(n_samples, 0, 1)                  # Feature 1
  X2 <- runif(n_samples, 0, 0.5)                # Feature 2
  X3 <- runif(n_samples, 0, 0.8)                # Feature 3
  X <- cbind(I, X1, X2, X3)

  #generating y
  y <- X %*% beta

  # Define the objective function
  f <- function(beta) sum((y - X %*% beta)^2)
  initial_point <- c(5, 2, 3, 2)

  D <- 5  # Diameter of the search space
  Hessian <- 2 * t(X) %*% X       #hessian for regression case
  eigenvalues <- eigen(Hessian)$values
  eigen_max <- max(eigenvalues)
  epsilon <- 0.1  # Desired optimization accuracy

  # Run beta_NGD_optimum
  result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon, f)

  # Invalid initial_point
  expect_error(beta_NGD_optimum(NULL, D, eigen_max, epsilon, f), "initial_point must be a non-empty numeric vector")

  # Invalid D
  expect_error(beta_NGD_optimum(initial_point, -5, eigen_max, epsilon, f), "D must be a positive numeric value")

  # Invalid eigen_max
  expect_error(beta_NGD_optimum(initial_point, D, -7, epsilon, f), "eigen_max must be a positive numeric value")

  # Invalid epsilon
  expect_error(beta_NGD_optimum(initial_point, D, eigen_max, -0.7, f), "epsilon must be a positive numeric value")

  # Invalid f
  expect_error(beta_NGD_optimum(initial_point, D, eigen_max, epsilon, "not a function"), "f must be a valid R function")
})

test_that("beta_NGD_optimum converges to a point near the minimum", {
  #no of samples
  n_samples <- 100
  #beta to generate data
  beta <- c(4,3,2,3)
  I <- rep(1, n_samples)                        # Intercept term
  X1 <- runif(n_samples, 0, 1)                  # Feature 1
  X2 <- runif(n_samples, 0, 0.5)                # Feature 2
  X3 <- runif(n_samples, 0, 0.8)                # Feature 3
  X <- cbind(I, X1, X2, X3)

  #generating y
  y <- X %*% beta

  # Define the objective function
  f <- function(beta) sum((y - X %*% beta)^2)
  initial_point <- c(5, 2, 3, 2)

  D <- 5  # Diameter of the search space
  Hessian <- 2 * t(X) %*% X       #hessian for regression case
  eigenvalues <- eigen(Hessian)$values
  eigen_max <- max(eigenvalues)
  epsilon <- 0.1  # Desired optimization accuracy

  # Run beta_NGD_optimum
  result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon, f)

  # Check that the optimum is close to the true minimum
  expect_true(all(abs(result$optimum - beta) < 0.1))
})
