#' Perform Sign Recovery with a Objective Function
#'
#'  It performs sign recovery by repeatedly comparing two points using the provided objective function.
#'
#' @useDynLib DuelingOptimization
#' @import Rcpp
#' @param x A numeric vector representing the first point.
#' @param y A numeric vector representing the second point.
#' @param delta A positive numeric value specifying the confidence parameter.
#' @param f A function representing the objective function. This function
#'   must take a numeric vector as input and return a scalar numeric value.
#' @return An integer: +1 if the recovered sign is positive, -1 otherwise.
#' @examples
#' # Define an objective function
#' f <- function(x) sum(x^3-x)
#' # Perform sign recovery
#' signRecovery(x = c(1, 2), y = c(3, 4), delta = 0.01, f = f)
#' @export
signRecovery <- function(x, y, delta, f) {
  #Input checks
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric vectors.")
  }
  if (length(x) != length(y)) {
    stop("x and y must have the same length.")
  }
  if (!is.function(f)) {
    stop("f must be a valid R function.")
  }
  if (!is.numeric(delta) || delta <= 0) {
    stop("delta must be a positive numeric value.")
  }
  #Calling RCPP function
  out <- signRecovery_c(x, y, delta, f)
  #return the recovered sign
  return(out)
}
