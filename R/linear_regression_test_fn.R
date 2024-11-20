set.seed(42)

# Generate synthetic data
n_samples <- 100

# Covariates (features)
I <- rep(1, n_samples)
X1 <- runif(n_samples, 0, 1)  # Feature 1
X2 <- runif(n_samples, 0, 0.5)   # Feature 2
X3 <- runif(n_samples, 0, 0.8)   # Feature 2
X <- cbind(I, X1, X2, X3)
beta <- c(5, 1, 2, 1)

Hessian <- 2 * t(X) %*% X

# Compute eigenvalues of the Hessian
eigenvalues <- eigen(Hessian)$values

# Find the maximum eigenvalue
max_eigenvalue <- max(eigenvalues)
min_eigenvalue <- min(eigenvalues)

# Response variable with some noise
y <- X%*%beta + rnorm(n_samples, mean = 0, sd = 0.4)
# y <- X%*%beta
# Combine into a data frame
data <- data.frame(X1 = X1, X2 = X2, X3 = X3, y = y)

# Fit a linear regression model
model <- lm(y ~ ., data = data)
print(coef(model))

f_value <- function(beta) {
  # Function to calculate the value of f at point x (for tracking purposes)
  return(sum((y-X%*%beta)^2))  # Example: simple quadratic function for illustration
  #Change according to your objective function
  #Example for linear regression,logistic regression, huber loss
}

#plotting
library(ggplot2)
f_array= beta_NGD_optimum_seq(c(4, 2, 3, 2), 5, max_eigenvalue)
f_data <- data.frame(
  Index = 0:(length(f_array)-1),
  Value = f_array
)

# Plot using ggplot2
ggplot(f_data, aes(x = Index, y = Value)) +
  geom_line(color = "blue") +           # Line connecting points
  # geom_point(color = "red", size = 0.0001) + # Points on the line
  labs(title = "RSS vs Iterations",
       x = "Iterations",
       y = "RSS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title
