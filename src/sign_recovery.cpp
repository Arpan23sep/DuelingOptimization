#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
int signRecovery_c(NumericVector x, NumericVector y, double delta, Function f) {
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

    double f_x = as<double>(f(x));
    double f_y = as<double>(f(y));
    int ot = (f_x < f_y) ? 1 : -1;

    // Aggregate feedback
    w += ot;

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
