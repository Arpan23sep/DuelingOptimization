#' Normalized Gradient Descent with Preference Feedback
#'
#' Implements a normalized gradient descent algorithm based on preference (dueling) feedback.
#' The function attempts to find an optimal point in a given space by iteratively updating
#' the initial point in the direction estimated from comparison feedback.
#'
#' @param initial_point A numeric vector representing the d-dimensional initial point.
#' @param eta A numeric value representing the learning rate (step size) to control the update size at each iteration.
#' @param gamma A numeric value representing the perturbation parameter, used to generate nearby points
#'   to probe the function's landscape.
#' @param T An integer representing the maximum number of steps the algorithm will take.
#' @param f A function representing the objective function to be minimized. This function
#'   must take a numeric vector as input and return a numeric value.
#' @return A list with the following components:
#'   \item{optimum}{A numeric vector representing the best point found after `T` iterations.}
#'   \item{f_array}{A numeric vector containing the best function value observed at each iteration.}
#' @export
#'
#' @examples
#' f <- function(x) sum(x^2)  # Example quadratic function
#' initial_point <- c(1, 1)
#' eta <- 0.01
#' gamma <- 0.1
#' T <- 100
#' result <- beta_NGD(initial_point, eta, gamma, T, f)
#' print(result)
#' print(result$f_array)
beta_NGD <- function(initial_point, eta, gamma, T, f) {
  # Initialize variables
  x <- initial_point
  f_array <- c()
  x_best <- x  # Track the best point found
  f_best <- f(x)  # Initial best function value
  f_array <- c(f_array, f_best)

  for (t in 1:T) {
    # Generate a random unit vector in d dimensions
    u <- rnorm(length(x))
    u <- u / sqrt(sum(u^2))

    # Compute perturbed points for comparison feedback
    x_plus <- x + gamma * u
    x_minus <- x - gamma * u

    # Simulate comparison feedback
    feedback <- compare_points(x_plus, x_minus, f)

    # Estimate the gradient direction
    gradient_estimate <- feedback * u

    # Update the current point
    x <- x + eta * gradient_estimate

    # Update the best point found so far
    f_current <- f(x)
    if (f_current < f_best) {
      f_best <- f_current
      x_best <- x
    }

    # Record the best function value
    f_array <- c(f_array, f_best)
  }

  # Return the best point and the function value array
  return(list(optimum = x_best, f_array = f_array))
}

#' Optimized Normalized Gradient Descent with Parameter Tuning
#'
#' Computes the parameters for normalized gradient descent based on the strong convexity
#' and smoothness properties of the objective function.
#'
#' @param initial_point A numeric vector representing the d-dimensional initial point.
#' @param D A numeric value representing the diameter of the search space.
#' @param eigen_max A numeric value representing the maximum eigenvalue of the Hessian (smoothness parameter).
#' @param epsilon A numeric value representing the desired optimization accuracy.
#' @param f A function representing the objective function to be minimized. This function
#'   must take a numeric vector as input and return a numeric value.
#' @return A list with the following components:
#'   \item{optimum}{A numeric vector representing the best point found.}
#'   \item{f_array}{A numeric vector containing the best function value observed at each iteration.}
#' @export
#'
#' @examples
#' f <- function(x) sum(x^2)  # Example quadratic function
#' initial_point <- c(1, 1)
#' D <- 10
#' eigen_max <- 5
#' epsilon <- 0.1
#' result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon, f)
#' print(result$optimum)
#' print(result$f_array)
beta_NGD_optimum <- function(initial_point, D, eigen_max, epsilon = 0.1, f) {
  # Compute parameters for the first phase
  d <- length(initial_point)
  beta <- eigen_max
  eta <- sqrt(epsilon) / (20 * sqrt(d * beta))
  T <- d * beta * D / epsilon
  gamma <- (epsilon / beta)^(3 / 2) / (240 * sqrt(2) * d * (D + eta * T)^2 *
                                         sqrt(log(480 * sqrt(beta * d) * (D + eta * T) / sqrt(2 * epsilon))))

  # Run the beta_NGD algorithm
  return(beta_NGD(initial_point, eta, gamma, T, f))
}

#' Compare Two Points Based on Objective Function
#'
#' Compares two points based on the values of a given objective function.
#' The function returns +1 if the objective function value at the first point
#' is smaller than the value at the second point, and -1 otherwise.
#'
#' @param x1 A numeric vector representing the first point.
#' @param x2 A numeric vector representing the second point.
#' @param f The objective function to be minimized. It must take a numeric vector as input and return a numeric value.
#' @return An integer, +1 if \code{f(x1) < f(x2)}, -1 otherwise.
#' @examples
#' f <- function(x) sum(x^2)
#' compare_points(c(1, 2), c(3, 4), f)
#' @export
compare_points <- function(x1, x2, f) {
  if (f(x1) < f(x2)) return(1)
  else return(-1)
}
