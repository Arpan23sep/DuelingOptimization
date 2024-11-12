# objective function for MSE (Linear Regression)
mse_loss <- function(beta, X, y) {
  residuals <- y - X %*% beta
  return(mean(residuals^2))
}

# Comparison function for beta_NGD
# Required for dueling comparison
compare_points <- function(beta1, beta2, X, y) {
  if (mse_loss(beta1, X, y) < mse_loss(beta2, X, y)) {
    return(1)
  } else {
    return(-1)
  }
}

# test function using the alpha_beta_NGD on linear regression
test_alpha_beta_NGD <- function() {
  # Generate synthetic linear regression data
  set.seed(12)
  n <- 100  # Number of data points
  d <- 2    # Number of features
  X <- matrix(rnorm(n * d), n, d)   # Feature matrix
  true_beta <- c(3, -2)             # True coefficients
  y <- X %*% true_beta + rnorm(n)   # Response variable with noise

  # Set initial parameters for alpha_beta_NGD
  initial_point <- rep(0, d)
  alpha <- 0.1                      # Strong convexity parameter
  beta <- 1.0                       # Smoothness parameter
  tolerance <- 1e-4                 # Desired accuracy

  # Run the (alpha, beta)-NGD algorithm
  result <- alpha_beta_NGD(initial_point, alpha, beta, tolerance)

  # Print the estimated coefficients and compare with true beta
  cat("Estimated coefficients using alpha_beta_NGD:\n")
  print(result)
  cat("True coefficients:\n")
  print(true_beta)
}

# Call the test function
test_alpha_beta_NGD()
