#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate the orientation of 3 points
int orientation(NumericVector p, NumericVector q, NumericVector r) {
  double val = (q[1] - p[1]) * (r[0] - q[0]) - (q[0] - p[0]) * (r[1] - q[1]);
  if (val == 0) return 0;  // Collinear
  return (val > 0) ? 1 : 2; // Clockwise or Counterclockwise
}

// Lower hull calculation function (baseline detection)
// [[Rcpp::export]]
NumericMatrix convex_hull_cpp(NumericMatrix points) {
  int n = points.nrow();
  if (n < 3) stop("At least 3 points are required");

  // Sort points by x-values (wavelengths)
  std::vector<std::pair<double, double>> sorted_points(n);
  for (int i = 0; i < n; ++i) {
    sorted_points[i] = std::make_pair(points(i, 0), points(i, 1));
  }
  std::sort(sorted_points.begin(), sorted_points.end());

  std::vector<int> lower_hull;

  // Compute lower hull only
  for (int i = 0; i < n; ++i) {
    while (lower_hull.size() >= 2 && orientation(
      NumericVector::create(sorted_points[lower_hull[lower_hull.size() - 2]].first, sorted_points[lower_hull[lower_hull.size() - 2]].second),
      NumericVector::create(sorted_points[lower_hull[lower_hull.size() - 1]].first, sorted_points[lower_hull[lower_hull.size() - 1]].second),
      NumericVector::create(sorted_points[i].first, sorted_points[i].second)) != 2) {
      lower_hull.pop_back();
    }
    lower_hull.push_back(i);
  }

  // Convert lower hull points back to NumericMatrix
  NumericMatrix hull_points(lower_hull.size(), 2);
  for (size_t i = 0; i < lower_hull.size(); ++i) {
    hull_points(i, 0) = sorted_points[lower_hull[i]].first;
    hull_points(i, 1) = sorted_points[lower_hull[i]].second;
  }

  return hull_points;
}
