# DuelingOptimization

DuelingOptimizationR is an R package that implements algorithms for convex optimization with preference-based (dueling) feedback, based on the paper "Dueling Convex Optimization" by Saha et al. This package provides tools for performing convex optimization using only noisy binary feedback on pairs of decision points, as opposed to direct gradient or function value information. The algorithms are suitable for applications like recommender systems, ranking, and online learning, where preference feedback is commonly available.

#Overview This package includes two primary functions:

-   beta_Normalized_Gradient: Implements a normalized gradient descent algorithm for general β-smooth convex functions using binary feedback.
-   alpha_beta_NGD: Extends the gradient descent to strongly convex and β-smooth functions, achieving faster convergence.

The core of these algorithms lies in estimating gradient directions based on noisy, preference-based comparisons. This enables optimization in scenarios where only binary feedback is available for queried pairs of points.

#Usage

- 1. beta_Normalized_Gradient:
beta_NGD performs gradient descent for β-smooth convex functions based solely on dueling (comparison-based) feedback.

result <- beta_NGD(initial_point, eta, gamma, T)

#Arguments
- initial_point: A numeric vector representing the starting point for the optimization.
- eta: The step size for gradient descent.
- gamma: The perturbation parameter to calculate comparison-based feedback.
- T: The number of iterations.

- alpha_beta_NGD
alpha_beta_NGD extends beta_NGD for optimizing α-strongly convex and β-smooth functions by running multiple phases of gradient descent. Each phase refines the accuracy and improves convergence rates.

