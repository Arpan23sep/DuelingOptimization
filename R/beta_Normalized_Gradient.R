#x_1 -intial d-dimentional intial point
#\eta - learning rate (step size) to control how much the point updates each iteration.
#\gamma - (peterbation parameter) used to generate nearby points to probe the function's landscape.
# T - maximum number of steps the algorithm will take.

compare_points <- function(x1, x2) {
  # This function should return +1 if f(x1) < f(x2), -1 otherwise
  # Replace this with real preference feedback or a function proxy.
  return(sample(c(-1, 1), 1))  # Random feedback for illustration
}

f_value <- function(x) {
  # Function to calculate the value of f at point x (for tracking purposes)
  return(sum(x^2))  # Example: simple quadratic function for illustration
}

beta_NGD <- function(initial_point, eta, gamma, T) {
  # Initialize variables
  x <- initial_point
  x_best <- x  # Track the best point found
  f_best <- Inf  # Placeholder for best function value

  for (t in 1:T) {
    # Generate a random unit vector in d dimensions
    u <- rnorm(length(x))
    u <- u / sqrt(sum(u^2))

    # Compute perturbed points for comparison feedback
    x_plus <- x + gamma * u
    x_minus <- x - gamma * u
  }
  return(x_best)
}


