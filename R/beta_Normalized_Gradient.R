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
#' result <- beta_NGD(initial_point, eta, gamma, T,f)
#' print(result)

beta_NGD <- function(initial_point, eta, gamma, T,f) {
  # Initialize variables
  x <- initial_point
  f_array<- c()
  x_best <- x  # Track the best point found
  f_best <- f(x)
  f_array<- c(f_array, f_best)# Placeholder for best function value
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
    feedback <- compare_points(x_plus, x_minus, f)

    # Estimate the gradient direction
    gradient_estimate <- feedback * u

    # Update the current point in the negative gradient direction
    x <- x + eta * gradient_estimate

    # Track the best point found so far
    # Assuming a function f_value that returns the function value at x
    f_current <- f(x)
    if (f_current < f_best) {
      f_best <- f_current
      x_best <- x
    }
    #print(paste0("Completed ", 100*t/T, "%"))
    f_array<- c(f_array, f_best)
  }

  # Return the best point found
  return(list("optimum"=x_best, "f_array"= f_array))
}

#'
#' @param initial_point A numeric vector representing the d-dimensional initial point.
#' @param D A numeric value representing the diameter of the search space.
#' @param eigen_max A numeric value representing the maximum eigenvalue of the Hessian (smoothness parameter).
#' @param epsilon A numeric value representing the desired optimization accuracy.
#' @return A list with the following components:
#'   \item{optimum}{A numeric vector representing the best point found.}
#'   \item{f_array}{A numeric vector containing the best function value observed at each iteration.}
#' @export
#'
#' @examples
#' # Example usage
#' initial_point <- c(1, 1)
#' D <- 10
#' eigen_max <- 5
#' epsilon <- 0.1
#' result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon)
#' print(result$optimum)
#' print(result$f_array)
beta_NGD_optimum <- function(initial_point, D, eigen_max, epsilon = 0.1,f) {
  d= length(initial_point)
  beta= eigen_max
  eta= sqrt(epsilon)/(20*sqrt(d*beta))
  T= d*beta*D/epsilon
  gamma= (epsilon/beta)^(3/2)/(240*sqrt(2)*d*(D+eta*T)^2*sqrt(log(480*sqrt(beta*d)*(D+ eta*T)/sqrt(2*epsilon))))
  return(beta_NGD(initial_point, eta, gamma, T,f))
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
 compare_points <- function(x1, x2, f) {
#   # This function should return +1 if f(x1) < f(x2), -1 otherwise
  if (f(x1) < f(x2)){
     return(1)
   } else {
     return(-1)
   }
 }

#' Calculate the Function Value at a Point
#'
#' A simple function to calculate the value of `f` at a given point. This is used
#' for tracking purposes in the `beta_NGD` algorithm.
#'
#' @param x A numeric vector representing the point at which to evaluate `f`.
#' @return A numeric value representing the function value at `x`.
#' @export
# f_value <- function(x) {
#   # Function to calculate the value of f at point x (for tracking purposes)
#   return(sum(x^2))  # Example: simple quadratic function for illustration
#   #Change according to your objective function
   #Example for linear regression,logistic regression, huber loss
# }
