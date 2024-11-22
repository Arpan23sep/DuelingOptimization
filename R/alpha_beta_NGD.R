#' (α, β)-Normalized Gradient Descent (NGD)
#'
#' This algorithm performs gradient descent on an \eqn{α}-strongly convex and \eqn{β}-smooth function.
#' It iteratively refines the solution over multiple phases, leveraging both the strong convexity
#' and smoothness properties to achieve faster convergence.
#'
#' @param initial_point A numeric vector representing the initial starting point in \eqn{R^d}.
#' @param alpha A numeric value representing the strong convexity parameter (\eqn{α}).
#' @param beta A numeric value representing the smoothness parameter (\eqn{β}).
#' @param D A numeric value representing the diameter of the search space.
#' @param tolerance A numeric value representing the desired accuracy for convergence. Default is 0.1.
#' @param f A function representing the objective function to be minimized. This function
#'   must take a numeric vector as input and return a numeric value.
#' @return A numeric vector representing the approximate optimal point after performing
#'         multiple phases of gradient descent. The returned point reduces the initial
#'         distance to the optimal point within the specified \code{tolerance}.
#' @export
#'
#' @details
#' The (α, β)-Normalized Gradient Descent (NGD) algorithm is designed for functions that are
#' both \eqn{α}-strongly convex and \eqn{β}-smooth. The algorithm progresses over several phases, where
#' each phase has its own step size, perturbation parameter, and query budget. The algorithm
#' aims to reduce the initial distance from the optimal point to within a specified \code{tolerance}.
#'
#' This implementation uses \code{beta_NGD} for the optimization steps within each phase.
#' It assumes the provided objective function \code{f} satisfies the strong convexity
#' and smoothness conditions for accurate results.
#'
#' @examples
#' # Define the objective function
#' f <- function(x) sum(x^2)  # Example quadratic function
#'
#' # Set parameters
#' initial_point <- c(0, 0)
#' alpha <- 0.1
#' beta <- 1.0
#' D <- 2
#' tolerance <- 0.01
#'
#' # Run the algorithm
#' result <- alpha_beta_NGD(initial_point, alpha, beta, D, tolerance, f)
#' print(result)
alpha_beta_NGD <- function(initial_point, alpha, beta, D, tolerance = 0.1, f) {

  # Initialize variables
  x_1 <- initial_point
  d <- length(x_1)
  phase_count <- ceiling(log2(alpha / tolerance))  # Number of phases needed

  # Set parameters for the first phase
  t <- 800 * d * beta / ((sqrt(2) - 1) * alpha)
  t_1 <- t * D
  epsilon_1 <- 400 * d * beta * D / ((sqrt(2) - 1) * t_1)
  eta_1 <- sqrt(epsilon_1) / (20 * sqrt(d * beta))
  gamma_1 <- (epsilon_1 / beta)^(3 / 2) / (240 * sqrt(2) * d * (D + eta_1 * t_1)^2 *
                                             sqrt(log(480 * sqrt(beta * d) * (D + eta_1 * t_1) / (sqrt(2) * epsilon_1))))

  # Initial descent using Algorithm 1
  x <- beta_NGD(x_1, eta_1, gamma_1, t_1, f)$optimum

  # Main loop for each phase
  for (k in 2:phase_count) {
    # Set parameters for current phase
    t_k <- 2 * t
    epsilon_k <- 400 * d * beta / ((sqrt(2) - 1) * t_k)
    eta_k <- sqrt(epsilon_k) / (20 * sqrt(d * beta))
    gamma_k <- (epsilon_k / beta)^(3 / 2) / (240 * sqrt(2) * d * (1 + eta_k * t_k)^2 *
                                               sqrt(log(480 * sqrt(beta * d) * (1 + eta_k * t_k) / (sqrt(2) * epsilon_k))))

    # Update current point using beta_NGD for the current phase
    x <- beta_NGD(x, eta_k, gamma_k, t_k, f)$optimum
    print(paste0(k / phase_count * 100, "% Done!"))
  }

  return(x)
}
