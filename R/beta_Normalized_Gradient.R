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
#' @return A numeric vector representing the best point found after `T` iterations.
#' @export
#'
#' @examples
#' initial_point <- c(1, 1)
#' eta <- 0.01
#' gamma <- 0.1
#' T <- 100
#' result <- beta_NGD(initial_point, eta, gamma, T)
#' print(result)
beta_NGD <- function(initial_point, eta, gamma, T) {
  # Initialize variables
  x <- initial_point
  x_best <- x  # Track the best point found
  f_best <- f_value(x)  # Placeholder for best function value

  for (t in 1:T) {
    # Generate a random unit vector in d dimensions
    u <- rnorm(length(x))
    u <- u / sqrt(sum(u^2))

    # Compute perturbed points for comparison feedback
    x_plus <- x + gamma * u
    x_minus <- x - gamma * u

    # Simulate comparison feedback (in practice, use real feedback)
    # For this implementation, assume a function compare_points is available
    # which returns +1 if f(x_plus) < f(x_minus), -1 otherwise.
    feedback <- compare_points(x_plus, x_minus)

    # Estimate the gradient direction
    gradient_estimate <- feedback * u

    # Update the current point in the negative gradient direction
    x <- x - eta * gradient_estimate

    # Track the best point found so far
    # Assuming a function f_value that returns the function value at x
    f_current <- f_value(x)
    if (f_current >= f_best) {
      f_best <- f_current
      x_best <- x
    }
  }

  # Return the best point found
  return(x_best)
}


#' Compare Points Based on Preference Feedback
#'
#' A placeholder function that simulates comparison feedback between two points.
#' Returns +1 if `f(x1) < f(x2)`, -1 otherwise.
#'
#' @param x1 A numeric vector representing the first point.
#' @param x2 A numeric vector representing the second point.
#' @return An integer, +1 if `f(x1) < f(x2)`, -1 otherwise.
#' @export
compare_points <- function(x1, x2) {
  # This function should return +1 if f(x1) < f(x2), -1 otherwise
  # Replace this with real preference feedback or a function proxy.
  return(sample(c(-1, 1), 1))  # Random feedback for illustration
}

#' Calculate the Function Value at a Point
#'
#' A simple function to calculate the value of `f` at a given point. This is used
#' for tracking purposes in the `beta_NGD` algorithm.
#'
#' @param x A numeric vector representing the point at which to evaluate `f`.
#' @return A numeric value representing the function value at `x`.
#' @export
f_value <- function(x) {
  # Function to calculate the value of f at point x (for tracking purposes)
  return(sum(x^2))  # Example: simple quadratic function for illustration
}
