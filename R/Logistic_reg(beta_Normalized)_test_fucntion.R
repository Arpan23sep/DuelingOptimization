# Generate synthetic data for logistic regression
set.seed(42)
n <- 100  # Number of data points
d <- 2    # Number of features

# Generate random features
X <- matrix(rnorm(n * d), n, d)
# Generate labels with a true beta
true_beta <- c(1, -1)
prob <- 1 / (1 + exp(-X %*% true_beta))
y <- rbinom(n, 1, prob)  # Binary labels

# Define the logistic loss function as the objective
f_value <- function(beta) {
  # Compute the logistic loss
  linear_comb <- X %*% beta
  loss <- -mean(y * log(1 / (1 + exp(-linear_comb))) + (1 - y) * log(1 - 1 / (1 + exp(-linear_comb))))
  return(loss)
}

# Comparison function to determine preference between two parameter vectors
compare_points <- function(beta1, beta2) {
  if (f_value(beta1) < f_value(beta2)) {
    return(1)
  } else {
    return(-1)
  }
}

# Initialize parameters
initial_point <- rep(0, d)  # Start from zero vector
eta <- 0.01                 # Learning rate
gamma <- 0.1                # Perturbation parameter
T <- 100                    # Number of iterations

# Run the beta_NGD function
result <- beta_NGD(initial_point, eta, gamma, T)
print("Estimated parameters using beta_NGD:")
print(result)

# Compare with logistic regression solution from glm for verification
glm_fit <- glm(y ~ X - 1, family = binomial)  # Fit logistic regression without intercept
print("True beta:")
print(true_beta)
print("GLM estimated beta:")
print(coef(glm_fit))
