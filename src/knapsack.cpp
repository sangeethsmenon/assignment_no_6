#include <Rcpp.h>
using namespace Rcpp;

int knapsack_brute_force_cpp(int W, NumericVector weights, NumericVector values) {
  int n = weights.size();
  
  // Implementing the fast C++ version here
  std::vector<std::vector<int>> dp(W + 1, std::vector<int>(n + 1));
  
  for (int w = 0; w <= W; w++) {
    for (int i = 0; i <= n; i++) {
      if (w == 0 || i == 0)
        dp[w][i] = 0;
      else if (weights[i - 1] <= w)
        dp[w][i] = std::max(static_cast<int>(values[i - 1]) + dp[w - weights[i - 1]][i - 1], dp[w][i - 1]);
      else
        dp[w][i] = dp[w][i - 1];
    }
  }
  
  return dp[W][n];
}

// [[Rcpp::export]]
int knapsack_brute_force(int W, NumericVector weights, NumericVector values, bool fast = false) {
  if (fast) {
    return knapsack_brute_force_cpp(W, weights, values);
  } else {
    // Call the original R version of the knapsack function
    return knapsack_brute_force(W, weights, values);
  }
}
