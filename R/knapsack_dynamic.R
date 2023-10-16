#' Dynamic Programming Knapsack Solver
#'
#' This function solves the knapsack problem using dynamic programming.
#'
#' @param x A data.frame with 'v' (values) and 'w' (weights) variables.
#' @param W The knapsack size (maximum weight).
#'
#' @return A list containing the maximum knapsack value and a list of selected items with their values and weights.
#'
#' @examples
#' \dontrun{
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = n, replace = TRUE),
#'   v = runif(n = n, 0, 10000)
#' )
#' result <- knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)
#' cat("Maximum Value:", result$max_value, "\n")
#' cat("Selected Items:", result$selected_items, "\n")
#' }
#'
#' @export

knapsack_dynamic <- function(x, W) {
  n <- nrow(x)  # Number of items in the subset
  dp <- matrix(0, 2, W + 1)  # Create a 2-row rolling array
  if (W <= 0) {
    stop("Knapsack size 'W' must be a positive value.")
  }
  for (i in 1:n) {
    for (w in 0:W) {
      row1 <- i %% 2 + 1  # Use a rolling array to reduce memory usage
      row2 <- 3 - row1
      if (x[i, "w"] <= w) {
        dp[row1, w] <- max(dp[row2, w], dp[row2, w - x[i, "w"]] + x[i, "v"])
      } else {
        dp[row1, w] <- dp[row2, w]
      }
    }
  }
  
  # Reconstruct the solution (list of selected items)
  i <- n
  w <- W
  selected_items <- list()
  while (i > 0 && w > 0) {
    row1 <- i %% 2 + 1
    row2 <- 3 - row1
    if (dp[row1, w] != dp[row2, w]) {
      selected_items[[i]] <- list(value = x[i, "v"], weight = x[i, "w"])
      w <- w - x[i, "w"]
    }
    i <- i - 1
  }
  
  # Combine the selected items into a single list
  selected_items <- do.call(c, selected_items)
  
  # Return the maximum value and the list of selected items
  return(list(max_value = dp[n %% 2 + 1, W], selected_items = selected_items))
}


