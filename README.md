# DuelingOptimization

DuelingOptimizationR is an R package that implements algorithms for convex optimization with preference-based (dueling) feedback, based on the paper "Dueling Convex Optimization" by Saha et al. This package provides tools for performing convex optimization using only noisy binary feedback on pairs of decision points, as opposed to direct gradient or function value information. The algorithms are suitable for applications like recommender systems, ranking, and online learning, where preference feedback is commonly available.

# Overview : This package includes two primary functions:

-   beta_Normalized_Gradient: Implements a normalized gradient descent algorithm for general β-smooth convex functions using binary feedback.
-   alpha_beta_NGD: Extends the gradient descent to strongly convex and β-smooth functions, achieving faster convergence.

The core of these algorithms lies in estimating gradient directions based on noisy, preference-based comparisons. This enables optimization in scenarios where only binary feedback is available for queried pairs of points.

# Usage :

1.  beta_Normalized_Gradient: beta_NGD performs gradient descent for β-smooth convex functions based solely on dueling (comparison-based) feedback.

result \<- beta_NGD(initial_point, eta, gamma, T)

# Arguments :

initial_point: A numeric vector representing the starting point for the optimization. - eta: The step size for gradient descent. - gamma: The perturbation parameter to calculate comparison-based feedback. - T: The number of iterations.

# Return :

The best decision point found after T iterations.

-2 alpha_beta_NGD alpha_beta_NGD extends beta_NGD for optimizing α-strongly convex and β-smooth functions by running multiple phases of gradient descent. Each phase refines the accuracy and improves convergence rates.

result \<- alpha_beta_NGD(initial_point, alpha, beta, tolerance, max_iter = 1000)

# Arguments :

-   initial_point: A numeric vector for the starting point.
-   alpha: The strong convexity parameter.
-   beta: The smoothness parameter.
-   tolerance: Desired tolerance for convergence.
-   max_iter: Maximum number of iterations (default is 1000).

# Return :

The near-optimal decision point for the convex function.

# Example
Basic example for beta_NGD
# Define an initial point and parameters
initial_point <- c(1, 1)
eta <- 0.01
gamma <- 0.1
T <- 100

# Run the normalized gradient descent
optimal_point <- beta_NGD(initial_point, eta, gamma, T)
print(optimal_point)

# Basic example of alpha_beta_NGD
Set up parameters for strongly convex optimization
initial_point <- c(1, 1)
alpha <- 0.1
beta <- 0.5
tolerance <- 1e-4
max_iter <- 500

# Run the phase-based gradient descent
optimal_point <- alpha_beta_NGD(initial_point, alpha, beta, tolerance, max_iter)
print(optimal_point)

# Installation Instructure
You can install the DuelingOptimization package directly from GitHub:

install.packages("devtools")
devtools::install_github("Arpan23sep/DuelingOptimization")


# References :

If you use this package in your research, please cite the original paper:

Saha, A., Koren, T., & Mansour, Y. (2021). Dueling Convex Optimization. Proceedings of the 38th International Conference on Machine Learning.
