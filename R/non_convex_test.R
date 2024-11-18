# Define the function f(x) = x^2 + 3 * sin^2(x)
f_value <- function(x) {
  return(x^2 + 3 * sin(x)^2)
}

# Define the preference feedback function based on f(x)
compare_points <- function(x1, x2) {
  # Returns +1 if f(x1) < f(x2), otherwise -1
  if (f_value(x1) < f_value(x2)) {
    return(1)
  } else {
    return(-1)
  }
}

# Testing Algorithm 1 with the given function
test_algorithm_1 <- function() {
  # Parameters
  initial_point <- 5         # Starting point
  eta <- 0.1                 # Learning rate
  gamma <- 0.1               # Perturbation parameter
  T <- 50                    # Number of iterations

  # Run Algorithm 1
  result <- beta_NGD(
    initial_point = initial_point,
    eta = eta,
    gamma = gamma,
    T = T
  )

  # Print the result
  cat("Optimal point found:", result, "\n")
  cat("Function value at optimal point:", f_value(result), "\n")
}

# Run the test
test_algorithm_1()
