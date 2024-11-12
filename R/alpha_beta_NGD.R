#(α, β)-Normalized Gradient Descent (NGD)
#This algorithm is particularly for α-strongly convex and β-smooth functions.

#Inputs
#x - initial starting point on R^d
#D - distance between x_1 & optimial x
#K_{\epsilon} - phase counts / the no of phases needed
#T_{\epsilon} - Query budget
#\eta - learning rate (step size) to control how much the point updates each iteration.


alpha_beta_NGD <- function(initial_point, alpha, beta, tolerance) {

  # Initialize variables
  x_1 <- initial_point
  d <- length(x_1)
  D <- norm(x_1, type = "2")^2
  #D <- norm(x_1-x_s, type = "2")^2  # Initial distance, used for convergence checks
  phase_count <- ceiling(log2(alpha / tolerance))  # Number of phases needed
  t <- 800 * d * beta / ((sqrt(2) - 1) * alpha)
  t_1 <- t* norm(x_1-x_s, type = "2")^2
  epsilon_1 <- 400 * d * beta * D / ((sqrt(2) - 1) * t_1)
  eta_1 <- sqrt(epsilon_1) / (20 * sqrt(d*beta))
  gamma_1 <- (epsilon_1/ beta)^(3 / 2) / (240 * sqrt(2) * d * (D + eta_1*t_1)^2 * sqrt(log(480 * sqrt(beta * d)*(D + eta_1*t_1) / (sqrt(2) * epsilon_1))))
  x <- beta_NGD(x_1, eta_1, gamma_1, t_1)
  # Main loop for phases
  for (k in 2:phase_count) {

    # Set parameters for current phase
    t_k <- 2*t
    epsilon_k <- 400 * d * beta/ ((sqrt(2) - 1) * t_k)
    eta_k <- sqrt(epsilon_k) / (20 * sqrt(d*beta))
    gamma_k <- (epsilon_k / beta)^(3 / 2) / (240 * sqrt(2) * d * (1 + eta_k*t_k)^2 * sqrt(log(480 * sqrt(beta * d)*(1 + eta_k*t_k) / (sqrt(2) * epsilon_k))))
    # Call to Algorithm 1's function (needs to be implemented)
    # This would ideally return an updated point for phase k
    x <- beta_NGD(x, eta_k, gamma_k, t_k)
    print(paste0(k/phase_count*100,"% Done!"))
    # Update parameters for the next phase
    # if (norm(x - initial_point, type = "2") < tolerance) break
  }

  return(x)
}
