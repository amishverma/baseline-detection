#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate the orientation of 3 points
int orientation(NumericVector p, NumericVector q, NumericVector r) {
  double val = (q[1] - p[1]) * (r[0] - q[0]) - (q[0] - p[0]) * (r[1] - q[1]);
  if (val == 0) return 0;  // Collinear
  return (val > 0) ? 1 : 2; // Clockwise or Counterclockwise
}

// Convex hull calculation function
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

  std::vector<int> lower_hull, upper_hull;

  // Lower hull
  for (int i = 0; i < n; ++i) {
    while (lower_hull.size() >= 2 && orientation(
      NumericVector::create(sorted_points[lower_hull[lower_hull.size() - 2]].first, sorted_points[lower_hull[lower_hull.size() - 2]].second),
      NumericVector::create(sorted_points[lower_hull[lower_hull.size() - 1]].first, sorted_points[lower_hull[lower_hull.size() - 1]].second),
      NumericVector::create(sorted_points[i].first, sorted_points[i].second)) != 2) {
      lower_hull.pop_back();
    }
    lower_hull.push_back(i);
  }

  // Upper hull
  for (int i = n - 1; i >= 0; --i) {
    while (upper_hull.size() >= 2 && orientation(
      NumericVector::create(sorted_points[upper_hull[upper_hull.size() - 2]].first, sorted_points[upper_hull[upper_hull.size() - 2]].second),
      NumericVector::create(sorted_points[upper_hull[upper_hull.size() - 1]].first, sorted_points[upper_hull[upper_hull.size() - 1]].second),
      NumericVector::create(sorted_points[i].first, sorted_points[i].second)) != 2) {
      upper_hull.pop_back();
    }
    upper_hull.push_back(i);
  }

  // Combine lower and upper hull
  std::vector<int> full_hull(lower_hull.begin(), lower_hull.end());
  full_hull.insert(full_hull.end(), upper_hull.begin() + 1, upper_hull.end() - 1);

  // Convert hull points back to NumericMatrix
  NumericMatrix hull_points(full_hull.size(), 2);
  for (size_t i = 0; i < full_hull.size(); ++i) {
    hull_points(i, 0) = sorted_points[full_hull[i]].first;
    hull_points(i, 1) = sorted_points[full_hull[i]].second;
  }

  return hull_points;
}
