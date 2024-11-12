#Testing for beta_NGD

# Define an initial starting point and initialise the parameters
initial_point <- c(1, 1)
eta <- 0.01
gamma <- 0.1
T <- 100

# Run the normalized gradient descent
optimal_point <- beta_NGD(initial_point, eta, gamma, T)
print(optimal_point)


#Testing for alpha_beta_NGD

# Set up parameters for strongly convex optimization
initial_point <- c(1, 1)
alpha <- 0.01
beta <- 0.05
tolerance <- 1e-4
#max_iter <- 500
x_s <- c(1.5,1.5)

# Run the phase-based gradient descent
optimal_point1 <- alpha_beta_NGD(initial_point, alpha, beta, tolerance)
print(optimal_point1)


# Testing for sign-recovery
# Test points
x <- c(1, 2)
y <- c(2, 1)
delta <- 0.05  # Confidence level

# Run sign-recovery
result <- signRecovery(x, y, delta)
print(result)
