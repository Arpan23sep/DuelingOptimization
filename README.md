# DuelingOptimization

DuelingOptimization is an R package that implements algorithms for convex optimization with preference-based (dueling) feedback, based on the paper "Dueling Convex Optimization" by Saha et al. This package provides tools for performing convex optimization using only noisy binary feedback on pairs of decision points, as opposed to direct gradient or function value information. The algorithms are suitable for applications like recommender systems, ranking, and online learning, where preference feedback is commonly available.

## Overview

This package includes four primary functions:

-   beta_NGD: Implements a normalized gradient descent algorithm for general β-smooth convex functions using binary feedback.

-   beta_NGD_optimum: Computes the parameters for normalized gradient descent based on the strong convexity and smoothness properties of the objective function.

-   alpha_beta_NGD: Extends the gradient descent to strongly convex and β-smooth functions, achieving faster convergence.

-   signRecovery: It performs sign recovery by repeatedly comparing(resampling) two points using the provided objective function. The original function is written in RCPP.

-   The core of these algorithms lies in estimating gradient directions based on noisy, preference-based comparisons. This enables optimization in scenarios where only binary feedback is available for queried pairs of points.

## Usage

### 1. beta_NGD

Implements a normalized gradient descent algorithm based on preference (dueling) feedback. The function attempts to find an optimal point in a given space by iteratively updating the initial point in the direction estimated from comparison feedback.

#### Arguments

-   initial_point: A numeric vector representing the starting point for the optimization.

-   eta: The step size for gradient descent.

-   gamma: The perturbation parameter to calculate comparison-based feedback.

-   T: The number of iterations.

-   f: A convex function representing the objective function to be minimized

`result <- beta_NGD(initial_point, eta, gamma, T, f)`

#### Return:

A list with the following components:

optimum: A numeric vector representing the best point found after T iterations.

f_array: A numeric vector containing the best function value observed at each iteration.

#### Example

`#Define an initial point and parameters`

`initial_point <- c(1, 1)`

`eta <- 0.01`

`gamma <- 0.1`

`T <- 100`

`f <- function(x) sum(x^2)`

`optimal_point <- beta_NGD(initial_point, eta, gamma, T)`

`print(optimal_point)`

### 2. alpha_beta_NGD

This function extends *beta_NGD* for optimizing *α-strongly convex* and *β-smooth* functions by running multiple phases of gradient descent. Each phase refines the accuracy and improves convergence rates.

``` result <- alpha_beta_NGD(``initial_point, alpha, beta, D, tolerance, f``) ```

#### Arguments

-   initial_point: A numeric vector for the starting point.
-   alpha: The strong convexity parameter.
-   beta: The smoothness parameter.
-   tolerance: Desired tolerance for convergence.
-   f: A convex function representing the objective function to be minimized

#### Return:

The near-optimal decision point for the convex function.

#### Example

`#Set up parameters for strongly convex optimization`

`f <- function(x) sum(x^2) # quadratic function`

`initial_point <- c(1, 1)`

`alpha <- 0.1`

`beta <- 1.0`

`D <- 2`

`tolerance <- 0.01`

`#Run the phase-based gradient descent`

`optimal_point <- alpha_beta_NGD(initial_point, alpha, beta, D, tolerance, f)`

`print(optimal_point)`

### 3. beta_NGD_optimum

Computes the parameters for normalized gradient descent based on the strong convexity and smoothness properties of the objective function.

`result <- beta_NGD_optimum(initial_point, D, eigen_max, epsilon = 0.1, f)`

### Arguments:

-   initial_point: A numeric vector representing the d-dimensional initial point.

-   D: A numeric value representing the diameter of the search space.

-   eigen_max: A numeric value representing the maximum eigenvalue of the Hessian (smoothness parameter).

-   epsilon: A numeric value representing the desired optimization accuracy.(Default, epsilon = 0.01)

-   f: A function representing the objective function to be minimized. This function must take a numeric vector as input and return a numeric value.

#### Return:

A list with the following components:

-   optimum: A numeric vector representing the best point found.

-   f_array: A numeric vector containing the best function value observed at each iteration.

#### Example:

`initial_point <- c(1, 1)`

`D <- 4`

`eigen_max <- 2`

`epsilon <- 0.01`

`f <- function(x) sum(x^2)`

`optimal_point <- beta_NGD(initial_point, D, eigen_max, epsilon = 0.1, f)`

### 4. NN_optimization

This function optimizes a two-layer neural network with sigmoid activation using normalized gradient descent. The weights of the neural network are flattened for optimization and converted back to their original structure after optimization.

`result <- NN_optimization(initial_weights, X, y, eta, gamma, T)`

#### Arguments:

-   initial_weights: A list containing the initial weights and biases of the neural network:

-   W1: A matrix of dimensions n_features x n_hidden representing input to hidden layer weights.

-   b1: A vector of size n_hidden representing biases for the hidden layer.

-   W2: A matrix of dimensions n_hidden x n_output representing hidden to output layer weights.

-   b2: A scalar representing the bias for the output layer.

-   X: A numeric matrix of size n_samples x n_features representing input features.

-   y: A numeric vector of size n_samples representing the response variable.

-   eta: A numeric value representing the learning rate.

-   gamma: A numeric value representing the perturbation parameter.

-   T: An integer representing the maximum number of iterations for optimization.

#### Return:

A list containing:

-   weights: A list of optimized weights and biases structured as the input initial_weights.

-   loss_array: A numeric vector containing the loss values over iterations.

### 5.signRecovery

This function serves as a wrapper for the C++ function signRecovery_c. It performs sign recovery by repeatedly comparing two points using the provided objective function.

`result <- signRecovery(x, y, delta, f)`

#### Arguments:

`x: A numeric vector representing the first point.`

`y: A numeric vector representing the second point.`

`delta: A positive numeric value specifying the confidence parameter.`

`f: A function representing the objective function.`

#### Return:

An integer: +1 if the recovered sign is positive, -1 otherwise.

#### Example

`f <- function(x) sum(x^3-x)`

`# Perform sign recovery`

`signRecovery(x = c(1, 2), y = c(3, 4), delta = 0.01, f = f)`

### Installation Instruction

You can install the **DuelingOptimization** package directly from GitHub. First, make sure you have the **`devtools`** package installed:

`install.packages("devtools")`

Then, you can install **DuelingOptimization** from GitHub:

`devtools::install_github("Arpan23sep/DuelingOptimization")`

### References

If you use this package in your research, please cite the original paper:

Saha, A., Koren, T., & Mansour, Y. (2021). Dueling Convex Optimization. Proceedings of the 38th International Conference on Machine Learning.
