#Basic Example for beta_NGD

# Define an initial point and parameters
initial_point <- c(1, 1)
eta <- 0.01
gamma <- 0.1
T <- 100

# Run the normalized gradient descent
optimal_point <- beta_NGD(initial_point, eta, gamma, T)
print(optimal_point)


#Basic Example for alpha_beta_NGD

# Set up parameters for strongly convex optimization
initial_point <- c(1, 1)
alpha <- 0.1
beta <- 0.5
tolerance <- 1e-4
max_iter <- 500

# Run the phase-based gradient descent
optimal_point <- alpha_beta_NGD(initial_point, alpha, beta, tolerance, max_iter)
print(optimal_point)
