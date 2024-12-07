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
#' @param f A convex function representing the objective function to be minimized. This function
#'   must take a numeric vector as input and return a numeric value.
#' @return A list with the following components:
#'   \itemize{
#'      \item{optimum:} {A numeric vector representing the best point found after `T` iterations.}
#'      \item{f_array:} {A numeric vector containing the best function value observed at each iteration.}
#'    }
#' @import ggplot2
#' @export
#'
#' @examples
#' # Linear regression example with beta_NGD
#' library(ggplot2)
#' # Set random seed for reproducibility
#' set.seed(42)
#'
#' # Generate synthetic data
#' n_samples <- 100                              #no of sample
#' I <- rep(1, n_samples)                        # Intercept term
#' X1 <- runif(n_samples, 0, 1)                  # Feature 1
#' X2 <- runif(n_samples, 0, 0.5)                # Feature 2
#' X3 <- runif(n_samples, 0, 0.8)                # Feature 3
#' X <- cbind(I, X1, X2, X3)
#' beta <- c(5, 1, 2, 1)                         # True coefficients
#'
#' # Response variable with noise
#' y <- X %*% beta + rnorm(n_samples, mean = 0, sd = 0.4)
#' data <- data.frame(X1 = X1, X2 = X2, X3 = X3, y = y)
#'
#' # Fit a linear regression model
#' model <- lm(y ~ ., data = data)
#' print("lm output")
#' print(coef(model))
#'
#' # Define the objective function (Residual/error Sum of Squares)
#' f_value <- function(beta) {
#'   sum((y - X %*% beta)^2)
#' }
#'
#' # Apply beta_NGD Optimization
#' result <- beta_NGD(
#'  c(4, 2, 3, 2),
#'  c(0.003, 0.007, 0.005, 0.007),
#'  c(0.001, 0.002, 0.003, 0.004),
#'  5000,
#'  f_value
#' )
#'
#' # Extracting output
#' f_array <- result$f_array
#' optimum <- result$optimum
#'print("Estimated parameters using beta_NGD:")
#'print(optimum)
#'
#' # data frame created
#' f_data <- data.frame(
#'   Index = 0:(length(f_array) - 1),
#'   Value = f_array
#' )
#' # Plot the RSS over iterations
#' ggplot(f_data, aes(x = Index, y = Value)) +
#'   geom_line(color = "blue") +
#'   labs(title = "RSS vs Iterations",
#'        x = "Iterations",
#'        y = "RSS") +
#'   theme_minimal() +
#'   theme(plot.title = element_text(hjust = 0.5))
#'
beta_NGD <- function(initial_point, eta, gamma, T, f) {
  #Input checks
  if (!is.numeric(T) || T <= 0) {
    stop("T must be a positive integer.")
  }
  if (!is.function(f)) {
    stop("f must be a valid R function.")
  }
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

    # comparison feedback
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

    # Record the best function values observed at each iteration
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
#' @param epsilon A numeric value representing the desired optimization accuracy.(Default, epsilon = 0.01)
#' @param f A function representing the objective function to be minimized. This function
#'   must take a numeric vector as input and return a numeric value.
#' @return A list with the following components:
#'   \itemize{
#'     \item{optimum:} {A numeric vector representing the best point found.}
#'     \item{f_array:} {A numeric vector containing the best function value observed at each iteration.}
#'    }
#' @export
#'
#' @examples
#' # Logistic regression example with beta_NGD_optimum
#' # Set random seed for reproducibility
#' set.seed(501)
#'
#' # Generate data for logistic regression
#' n <- 100  # Number of data points
#' d <- 5     # Number of features
#' X <- matrix(rnorm(n * d), n, d)  # Generate random features
#'
#' # Generate binary labels with a true beta
#' true_beta <- c(1, 2, 3, 4, 5)
#' prob <- 1 / (1 + exp(-X %*% true_beta))
#' y <- rbinom(n, 1, prob)  # Binary labels
#'
#' # Define the logistic loss function as the objective function
#' f_value <- function(beta) {
#'   linear_comb <- X %*% beta
#'   loss <- -mean(y * log(1 / (1 + exp(-linear_comb))) +
#'                 (1 - y) * log(1 - 1 / (1 + exp(-linear_comb))))
#'   return(loss)
#' }
#'
#' # Initialize parameters
#' initial_point <- c(2, 1, 4, 3, 6)
#' D <- 10
#' Hessian <- t(X) %*% X      #Hessian for logistic regression
#' eigenvalues <- eigen(Hessian)$values
#' eigen_max <- 0.25 * max(eigenvalues)   #max eigenvalue of hessian
#'
#' # Run the beta_NGD_optimum function
#' result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon = 0.1, f_value)
#' point <- result$optimum
#' f_array <- result$f_array
#' print("Estimated parameters using beta_NGD_optimum:")
#' print(point)
#'
#' # Compare with logistic solution from glm
#' # Fit logistic regression without intercept
#' glm_fit <- glm(y ~ X - 1, family = binomial)
#' print("GLM estimated beta:")
#' print(coef(glm_fit))
#'
#' # Create a data frame for visualization
#' f_data <- data.frame(
#'   Index = 0:(length(f_array) - 1),
#'   Value = f_array
#' )
#'
#' # Plot negative log-likelihood vs iterations using ggplot2
#' library(ggplot2)
#' ggplot(f_data, aes(x = Index, y = Value)) +
#'   geom_line(color = "blue") +
#'   labs(title = "Negative log-likelihood vs Iterations",
#'        x = "Iterations",
#'        y = "Negative log-likelihood") +
#'   theme_minimal() +
#'   theme(plot.title = element_text(hjust = 0.5))

beta_NGD_optimum <- function(initial_point, D, eigen_max, epsilon = 0.1, f) {
  #Input check
  if (!is.numeric(initial_point) || length(initial_point) == 0) {
    stop("initial_point must be a non-empty numeric vector.")
  }
  if (!is.numeric(D) || D <= 0) {
    stop("D must be a positive numeric value.")
  }
  if (!is.numeric(eigen_max) || eigen_max <= 0) {
    stop("eigen_max must be a positive numeric value.")
  }
  if (!is.numeric(epsilon) || epsilon <= 0) {
    stop("epsilon must be a positive numeric value.")
  }
  if (!is.function(f)) {
    stop("f must be a valid R function.")
  }
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
#' @keywords internal
compare_points <- function(x1, x2, f) {
  # Check if x1 and x2 are numeric vectors
  if (!is.numeric(x1) || !is.numeric(x2)) {
    stop("x1 and x2 must be numeric vectors.")
  }

  # Check if x1 and x2 have the same length
  if (length(x1) != length(x2)) {
    stop("x1 and x2 must have the same length.")
  }

  # Check if f is a valid function
  if (!is.function(f)) {
    stop("f must be a valid R function.")
  }
  # Perform the comparison
  if (f(x1) < f(x2)) return(1)
  else return(-1)
}
