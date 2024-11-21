#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// Algorithm 3: Sign-Recovery for Noisy Comparison Feedback
// Given points x and y, this function recovers the true sign of f(x) - f(y)
// with high confidence by aggregating noisy feedback from repeated comparisons.

// [[Rcpp::export]]
int compare_points(NumericVector x1, NumericVector x2) {
  // Compare the values of f(x1) and f(x2)
  double f_x1 = sum(pow(x1, 2));  // Quadratic function f(x) = sum(x^2)
  double f_x2 = sum(pow(x2, 2));  // Quadratic function f(x) = sum(x^2)

  if (f_x1 < f_x2) {
    return 1;  // Return +1 if f(x1) < f(x2)
  } else {
    return -1; // Return -1 otherwise
  }
}

// [[Rcpp::export]]
int signRecovery(NumericVector x, NumericVector y, double delta) {
  // Validate inputs
  if (x.size() != y.size()) {
    stop("Input vectors x and y must have the same size.");
  }
  if (delta <= 0) {
    stop("Delta must be greater than zero.");
  }

  int w = 0;         // Cumulative feedback count
  int t = 0;         // Query counter
  double pt;         // Probability estimate for preference
  double conft;      // Confidence bound
  double lt_x_y, lt_y_x;

  // Main loop for querying the oracle
  while (true) {
    t += 1;

    // Compare points x & y
    int ot = compare_points(x, y);
    w += ot;  // Aggregate feedback

    // Calculate the probability estimate and confidence bound
    pt = static_cast<double>(w) / t;
    conft = sqrt(log(8.0 * t * t / delta) / (2.0 * t));

    // Calculate lower bounds for preference certainty
    lt_x_y = pt - conft;
    lt_y_x = 1.0 - pt - conft;

    // Stop if the preference is sufficiently certain
    if (lt_x_y > 0.5 || lt_y_x > 0.5) {
      break;
    }
  }

  // Determine the final preference based on cumulative feedback
  return (lt_x_y > 0.5) ? 1 : -1;
}

// [[Rcpp::export]]
double f1_value(NumericVector x) {
  // Function to calculate the value of f at point x
  return sum(pow(x, 2));  // Simple quadratic function f(x) = sum(x^2)
}
