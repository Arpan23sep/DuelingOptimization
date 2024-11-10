#(α, β)-Normalized Gradient Descent (NGD)
#This algorithm is particularly for α-strongly convex and β-smooth functions.

#Inputs
#x - initial starting point on R^d
#D - distance between x_1 & optimial x
#K_{\epsilon} - phase counts / the no of phases needed
#T_{\epsilon} - Query budget
#\eta - learning rate (step size) to control how much the point updates each iteration.


alpha_beta_NGD <- function(initial_point, alpha, beta, tolerance, max_iter = 1000) {

  # Initialize variables
  x <- initial_point
  D <- norm(x, type = "2")  # Initial distance, used for convergence checks
  phase_count <- ceiling(log2(alpha / tolerance))  # Number of phases needed
  t <- 800 * beta / ((sqrt(2) - 1) * alpha)
  eta <- sqrt(tolerance) / (20 * sqrt(beta))

  # Main loop for phases
  for (k in 1:phase_count) {
    # Set parameters for current phase
    epsilon_k <- tolerance / (2 ^ k)
    gamma <- (epsilon_k / beta)^(3 / 2) / (240 * sqrt(2) * D * log(480 * sqrt(beta * D) / (sqrt(2) * epsilon_k)))

    # Call to Algorithm 1's function (needs to be implemented)
    # This would ideally return an updated point for phase k
    x <- beta_NGD(x, eta, gamma, t)

    # Update parameters for the next phase
    if (norm(x - initial_point, type = "2") < tolerance) break
  }

  return(x)
}



