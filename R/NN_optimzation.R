#' Neural Network Optimization with Sigmoid activation
#'
#' This function optimizes a two-layer neural network with sigmoid activation using
#' normalized gradient descent.
#'
#' @param initial_weights A list containing the initial weights and biases of the neural network:
#'   \itemize{
#'     \item \code{W1}: A matrix of dimensions \code{n_features x n_hidden} representing input to hidden layer weights.
#'      \emph{(n_features is the number of features in the input dataset, and n_hidden is the number of hidden layer neurons.)}
#'     \item \code{b1}: A vector of size \code{n_hidden} representing biases for the hidden layer.
#'     \item \code{W2}: A matrix of dimensions \code{n_hidden x n_output} representing hidden to output layer weights.
#'      \emph{(n_output is typically 1 for regression problems or the number of classes for classification problems.)}
#'     \item \code{b2}: A scalar representing the bias for the output layer.
#'   }
#' @param X A numeric matrix of size \code{n_samples x n_features} representing input features.
#'    \emph{(n_samples is the number of observations or rows in the dataset, and n_features is the number of columns or features in the input dataset.)}
#' @param y A numeric vector of size \code{n_samples} representing the response variable.
#'  \emph{(n_samples should match the number of rows in \code{X}.)}
#' @param eta A numeric value representing the learning rate.
#' @param gamma A numeric value representing the perturbation parameter.
#' @param T An integer representing the maximum number of iterations for optimization.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{weights}: A list of optimized weights and biases structured as the input \code{initial_weights}.
#'     \item \code{loss_array}: A numeric vector containing the loss values over iterations.
#'   }
#' @export
#'
#' @examples
#' set.seed(567)
#'
#' # Generate data
#' n_samples <- 100
#' n_features <- 5
#' n_hidden <- 10
#'
#' #Generate design matrix
#' X <- matrix(rnorm(n_samples * n_features), n_samples, n_features)
#' true_weights <- list(
#'   W1 = matrix(runif(n_features * n_hidden), n_features, n_hidden),
#'   b1 = runif(n_hidden),
#'   W2 = matrix(runif(n_hidden), n_hidden, 1),
#'   b2 = runif(1)
#' )
#' #Sigmoid activation
#' hidden_layer <- 1 / (1 + exp(-(X %*% true_weights$W1 + true_weights$b1)))
#' output_layer <- hidden_layer %*% true_weights$W2 + true_weights$b2
#' # Generate y
#' y <- rowSums(output_layer)
#'
#' # Initialize neural network weights
#' initial_weights <- list(
#'   W1 = matrix(rnorm(n_features * n_hidden), n_features, n_hidden),
#'   b1 = rep(0, n_hidden),
#'   W2 = matrix(rnorm(n_hidden), n_hidden, 1),
#'   b2 = 0
#' )
#'
#' # Set optimization parameters
#' eta <- 0.01
#' gamma <- 0.1
#' T <- 10000
#'
#' # Run optimization
#' result <- NN_optimization(
#'   initial_weights = initial_weights,
#'   X = X,
#'   y = y,
#'   eta = eta,
#'   gamma = gamma,
#'   T = T
#' )
#'
#' library(ggplot2)
#' # Convert loss array to a data frame
#' loss_data <- data.frame(
#'  Iterations = seq_along(result$loss_array),
#'  Loss = result$loss_array
#')
#' # Create the ggplot
#' ggplot(loss_data, aes(x = Iterations, y = Loss)) +
#'  geom_line(color = "blue") +
#'  labs(
#'    title = "Loss Over Iterations",
#'    x = "Iterations",
#'    y = "Loss"
#'  ) +
#'  theme_classic() +
#'  theme(
#'    plot.title = element_text(hjust = 0.5)
#'  )

NN_optimization <- function(initial_weights, X, y, eta, gamma, T) {
  # Input checks
  if (!is.list(initial_weights)) {
    stop("initial_weights must be a list.")
  }

  if (!is.matrix(initial_weights$W1) || !is.numeric(initial_weights$W1)) {
    stop("initial_weights$W1 must be a numeric matrix.")
  }

  if (!is.numeric(initial_weights$b1) || length(initial_weights$b1) != ncol(initial_weights$W1)) {
    stop("initial_weights$b1 must be a numeric vector of length equal to the number of columns in W1.")
  }

  if (!is.matrix(initial_weights$W2) || !is.numeric(initial_weights$W2)) {
    stop("initial_weights$W2 must be a numeric matrix.")
  }

  if (nrow(initial_weights$W2) != ncol(initial_weights$W1)) {
    stop("The number of rows in W2 must match the number of columns in W1.")
  }

  if (!is.numeric(initial_weights$b2) || length(initial_weights$b2) != 1) {
    stop("initial_weights$b2 must be a numeric scalar.")
  }

  if (!is.matrix(X) || !is.numeric(X)) {
    stop("X must be a numeric matrix.")
  }

  if (ncol(X) != nrow(initial_weights$W1)) {
    stop("The number of columns in X must match the number of rows in W1.")
  }

  if (!is.numeric(y) || length(y) != nrow(X)) {
    stop("y must be a numeric vector with a length equal to the number of rows in X.")
  }

  if (!is.numeric(eta) || eta <= 0) {
    stop("eta must be a positive numeric value.")
  }

  if (!is.numeric(gamma) || gamma <= 0) {
    stop("gamma must be a positive numeric value.")
  }

  if (!is.numeric(T) || T <= 0 || T != as.integer(T)) {
    stop("T must be a positive integer.")
  }

  # Flatten the weights into a single vector for optimization
  flatten_weights <- function(weights) {
    c(as.vector(weights$W1), weights$b1, as.vector(weights$W2), weights$b2)
  }

  # Convert a flat vector back to a weights list
  unflatten_weights <- function(flat_weights, n_features, n_hidden, n_output) {
    W1 <- matrix(flat_weights[1:(n_features * n_hidden)], n_features, n_hidden)
    b1 <- flat_weights[(n_features * n_hidden + 1):(n_features * n_hidden + n_hidden)]
    W2 <- matrix(flat_weights[(n_features * n_hidden + n_hidden + 1):(n_features * n_hidden + n_hidden + n_hidden * n_output)], n_hidden, n_output)
    b2 <- flat_weights[length(flat_weights)]
    list(W1 = W1, b1 = b1, W2 = W2, b2 = b2)
  }

  # Define the loss function
  loss_fn <- function(flat_weights) {
    weights <- unflatten_weights(flat_weights, n_features = ncol(X), n_hidden = ncol(initial_weights$W1), n_output = 1)

    # Forward pass: Input to hidden layer
    H <- 1 / (1 + exp(-(X %*% weights$W1 + matrix(weights$b1, nrow = nrow(X), ncol = ncol(weights$W1), byrow = TRUE))))  # Sigmoid activation

    # Hidden layer to output
    predictions <- H %*% weights$W2 + weights$b2

    #Calculating Mean squared error
    mean((predictions - y)^2)
  }

  # Perform beta_NGD using the defined loss function
  initial_point <- flatten_weights(initial_weights)

  #Calling beta_NGD
  result <- beta_NGD(
    initial_point = initial_point,
    eta = eta,
    gamma = gamma,
    T = T,
    f = loss_fn
  )

  # Convert optimized weights back into the neural network structure
  optimized_weights <- unflatten_weights(result$optimum, n_features = ncol(X), n_hidden = ncol(initial_weights$W1), n_output = 1)

  return(list(weights = optimized_weights, loss_array = result$f_array))
}
