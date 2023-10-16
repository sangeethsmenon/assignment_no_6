#' Greedy Knapsack Solver
#'
#' This function solves the knapsack problem using a greedy approach by selecting items based on
#' their value-to-weight ratio.
#'
#' @param x A data.frame with 'v' (values) and 'w' (weights) variables.
#' @param W The knapsack size (maximum weight).
#'
#' @return A list containing the maximum knapsack value and indices of selected items.
#'
#' @examples
#' \dontrun{
#' RNGversion(min(as.character(getRversion()), "3.5.3"))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <- data.frame(
#'   w = sample(1:4000, size = n, replace = TRUE),
#'   v = runif(n = n, 0, 10000)
#' )
#' result <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' cat("Maximum Value:", result$value, "\n")
#' cat("Selected Elements:", result$elements, "\n")
#' }
#'
#' @export

greedy_knapsack <- function(x, W) {
  # Check if x is a data.frame with 'v' and 'w' variables
  if (!is.data.frame(x) || !all(c("v", "w") %in% colnames(x))) {
    stop("Input 'x' must be a data.frame with 'v' and 'w' variables.")
  }
  if (W <= 0) {
    stop("Knapsack size 'W' must be a positive value.")
  }
  # Check if 'v' and 'w' have only positive values
  if (any(x$v <= 0) || any(x$w <= 0)) {
    stop("Both 'v' and 'w' must have only positive values.")
  }
  
  n <- nrow(x)  # Number of items
  max_value <- 0
  best_elements <- integer(0)
  
  # Calculate value-to-weight ratio for each item
  x$ratio <- x$v / x$w
  
  # Create an index vector for sorting
  sorted_indices <- order(-x$ratio)
  x <- x[sorted_indices, ]
  
  total_weight <- 0
  i <- 1
  
  # Add items to the knapsack based on their value-to-weight ratio
  while (total_weight + x$w[i] <= W && i <= n) {
    best_elements <- c(best_elements, sorted_indices[i])
    total_weight <- total_weight + x$w[i]
    max_value <- max_value + x$v[i]
    i <- i + 1
  }
  
  # Ensure max_value is an integer without decimal places
  max_value <- round(max_value)
  
  # Return the result as a list
  result <- list(value = max_value, elements = best_elements)
  return(result)
}
