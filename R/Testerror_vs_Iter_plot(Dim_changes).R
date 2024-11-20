library(ggplot2)

set.seed(42)
epsilon <- 0.1

# Function to calculate RSS
f_value <- function(beta, X, y) {
  return(sum((y - X %*% beta)^2))  # Residual Sum of Squares (RSS)
}


# Generate data for multiple scenarios of beta length
n_samples <- 100
I <- rep(1, n_samples)
scenarios <- list(
  "2 features" = c(5, 1),
  "3 features" = c(5, 1, 2),
  "4 features" = c(5, 1, 2, 1),
  "5 features" = c(5, 1, 2, 1, 3),
  "6 features" = c(5, 1, 2, 1, 3, 4)
)
initials <- list(
  "2 features" = c(4, 2),
  "3 features" = c(4, 2, 3),
  "4 features" = c(4, 2, 3, 2),
  "5 features" = c(4, 2, 3, 2, 2),
  "6 features" = c(4, 2, 3, 2, 2, 5)
)
X_list <- list(
  "2 features" = cbind(I, replicate(1, runif(n_samples, 0, 1))),
  "3 features" = cbind(I, replicate(2, runif(n_samples, 0, 1))),
  "4 features" = cbind(I, replicate(3, runif(n_samples, 0, 1))),
  "5 features" = cbind(I, replicate(4, runif(n_samples, 0, 1))),
  "6 features" = cbind(I, replicate(5, runif(n_samples, 0, 1)))
)
T_max=0
f_data_list <- list()
for (scenario in names(scenarios)) {
  X <- X_list[[scenario]]
  beta <- scenarios[[scenario]]
  initial_point <- initials[[scenario]]
  d<- length(initial_point)
  D= sum((beta-initial_point)^2)+1
  eigen_max= max(eigen(2 * t(X) %*% X)$values)
  T= d*eigen_max*D/epsilon
  if (T> T_max){T_max=T}
}
for (scenario in names(scenarios)) {
  beta <- scenarios[[scenario]]
  n_features <- length(beta)

  # Generate synthetic data
  X <- X_list[[scenario]]
  y <- X %*% beta + rnorm(n_samples, mean = 0, sd = 0.4)

  # Compute RSS values
  initial_point= initials[[scenario]]
  d= length(initial_point)
  B= max(eigen(2 * t(X) %*% X)$values)
  eta= sqrt(epsilon)/(20*sqrt(d*B))
  D= sum((beta-initial_point)^2)+1
  T= T_max
  gamma= (epsilon/B)^(3/2)/(240*sqrt(2)*d*(D+eta*T)^2*sqrt(log(480*sqrt(B*d)*(D+ eta*T)/sqrt(2*epsilon))))
  f_array <- beta_NGD_seq(initial_point, eta, gamma, T)

  # Add to combined data frame
  f_data <- data.frame(
    Iteration = 0:(length(f_array) - 1),
    RSS = f_array,
    Scenario = scenario
  )
  f_data_list[[scenario]] <- f_data
  print(paste0("Completed ", scenario))
}

# Combine all data
f_data_combined <- do.call(rbind, f_data_list)

# Plotting
ggplot(f_data_combined, aes(x = Iteration, y = RSS, color = Scenario)) +
  geom_line() +
  labs(title = "RSS vs Iterations for Different Beta Lengths",
       x = "Iterations",
       y = "RSS",
       color = "Scenario") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
