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

initial_point <- 5
D <- 25
eigen_max <- 8
result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon=0.1)

# Print the result
cat("Optimal point found:", result, "\n")
cat("Function value at optimal point:", f_value(result), "\n")

