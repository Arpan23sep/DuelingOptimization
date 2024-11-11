#Define the Huber loss function
huber_loss <- function(beta, X, y, delta = 1.0) {
  residuals <- y - X %*% beta
  loss <- ifelse(abs(residuals) <= delta,
                 0.5 * residuals^2,
                 delta * abs(residuals) - 0.5 * delta^2)
  return(mean(loss))
}

# Comparison function for Huber loss between two points
compare_points <- function(beta1, beta2, X, y, delta = 1.0) {
  if (huber_loss(beta1, X, y, delta) < huber_loss(beta2, X, y, delta)) {
    return(1)
  } else {
    return(-1)
  }
}

# Generate synthetic data for testing
set.seed(42)
n <- 100  # Number of data points
d <- 2    # Number of features

X <- matrix(rnorm(n * d), n, d)        # Random features
true_beta <- c(2, -3)                  # True parameter vector
y <- X %*% true_beta + rnorm(n, 0, 1)  # Response with some noise

# Initialize parameters for Algorithm 1
initial_point <- rep(0, d)  # Starting point
eta <- 0.01                 # Learning rate
gamma <- 0.1                # Perturbation parameter
T <- 100                    # Number of iterations
delta <- 1.0                # Huber loss delta threshold

# Run Algorithm 1 (beta_NGD) with Huber loss
result <- beta_NGD(initial_point, eta, gamma, T, X, y, delta)

# Output the estimated parameters
cat("Estimated parameters using beta_NGD with Huber loss:\n")
print(result)
