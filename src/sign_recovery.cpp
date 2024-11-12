#include <Rcpp.h>
using namespace Rcpp;

// Algorithm 3: Sign-Recovery for Noisy Comparison Feedback
// Given points x and y, this function recovers the true sign of f(x) - f(y)
// with high confidence by aggregating noisy feedback from repeated comparisons.

// [[Rcpp::export]]
int signRecovery(NumericVector x, NumericVector y, double delta) {
  int w = 0;         // Cumulative feedback count
  int t = 0;         // Query counter
  double pt;         // Probability estimate for preference
  double conft;      // Confidence bound
  double lt_x_y, lt_y_x;

  // Main loop for querying the oracle
  //Using resampling trick
  while (true) {
    t += 1;

    // try to compare points x & y
    int ot = Rcpp::as<int>(Rcpp::Function("compare_points")(x, y));
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


