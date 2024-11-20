beta_NGD_seq <- function(initial_point, eta, gamma, T) {
  # Initialize variables
  x <- initial_point
  f_array<- c()
  x_best <- x  # Track the best point found
  #f_best <- f_value(x, X, y)  # Placeholder for best function value
  f_best <- f_value(x)
  f_array<- c(f_array, f_best)
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
    x <- x + eta * gradient_estimate

    # Track the best point found so far
    # Assuming a function f_value that returns the function value at x
    f_current <- f_value(x, X, y)
    if (f_current < f_best) {
      f_best <- f_current
      x_best <- x
    }
    f_array<- c(f_array, f_best)
  }

  return(f_array)
}

beta_NGD_optimum_seq<- function(initial_point, D, eigen_max, epsilon=0.1){
  d= length(initial_point)
  beta= eigen_max
  eta= sqrt(epsilon)/(20*sqrt(d*beta))
  T= d*beta*D/epsilon
  gamma= (epsilon/beta)^(3/2)/(240*sqrt(2)*d*(D+eta*T)^2*sqrt(log(480*sqrt(beta*d)*(D+ eta*T)/sqrt(2*epsilon))))
  return(beta_NGD_seq(initial_point, eta, gamma, T))
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
  if (f_value(x1, X, y) < f_value(x2, X, y)){
    return(1)
  } else {
    return(-1)
  }
}
