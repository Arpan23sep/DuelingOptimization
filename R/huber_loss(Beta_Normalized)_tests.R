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
