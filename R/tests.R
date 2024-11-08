#Basic Example for beta_NGD

# Define an initial point and parameters
initial_point <- c(1, 1)
eta <- 0.01
gamma <- 0.1
T <- 100

# Run the normalized gradient descent
optimal_point <- beta_NGD(initial_point, eta, gamma, T)
print(optimal_point)
