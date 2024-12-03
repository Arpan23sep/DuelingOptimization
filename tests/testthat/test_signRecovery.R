library(testthat)
library(DuelingOptimization)

test_that("signRecovery runs successfully and returns correct output", {
  # Objective function
  f <- function(x) sum(x^2)  # Quadratic function

  # Define input parameters
  x <- c(1, 2)
  y <- c(3, 4)
  delta <- 0.01          #Setting precision level

  # Run signRecovery
  result <- signRecovery(x = x, y = y, delta = delta, f = f)

  # Check output type
  expect_type(result, "integer")

  # validate result range/is it in our precision range or not
  expect_true(result %in% c(-1, 1))
})

test_that("signRecovery throws appropriate errors for invalid inputs", {
  f <- function(x) sum(x^2)

  # Test for non-numeric x or y
  expect_error(signRecovery(x = "a", y = c(1, 2), delta = 0.01, f = f),
               "Both x and y must be numeric vectors")
  expect_error(signRecovery(x = c(1, 2), y = "b", delta = 0.01, f = f),
               "Both x and y must be numeric vectors")

  # Test for mismatched lengths of x and y
  expect_error(signRecovery(x = c(1, 2), y = c(1, 2, 3), delta = 0.01, f = f),
               "x and y must have the same length")

  # Test for invalid delta
  expect_error(signRecovery(x = c(1, 2), y = c(3, 4), delta = -0.01, f = f),
               "delta must be a positive numeric value")
  expect_error(signRecovery(x = c(1, 2), y = c(3, 4), delta = "hiii", f = f),
               "delta must be a positive numeric value")

  # Test for invalid f
  expect_error(signRecovery(x = c(1, 2), y = c(3, 4), delta = 0.01, f = "function?"),
               "f must be a valid R function")
})

test_that("signRecovery integrates well with a known objective function", {
  f <- function(x) sum(x)

  # Case where f(x) < f(y)
  result1 <- signRecovery(x = c(1, 2), y = c(3, 4), delta = 0.01, f = f)
  expect_equal(result1, +1)  # Expect +1 as f(x) < f(y)

  # Case where f(x) > f(y)
  result2 <- signRecovery(x = c(3, 4), y = c(1, 2), delta = 0.01, f = f)
  expect_equal(result2, -1)  # Expect -1 as f(x) > f(y)

  # Case where f(x) == f(y)
  result3 <- signRecovery(x = c(1, 1), y = c(1, 1), delta = 0.01, f = f)
  expect_equal(result3, -1)
})
