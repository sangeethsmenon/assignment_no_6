#' Brute-Force Knapsack Solver
#'
#' This function solves the knapsack problem using a brute-force approach by
#' enumerating all different combinations of items.
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
#' result <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' cat("Maximum Value:", result$value, "\n")
#' cat("Selected Elements:", result$elements, "\n")
#' }
#'
#' @export
#' @import Rcpp
brute_force_knapsack <- function(x, W) {
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
  
  # Enumerate all different combinations using binary representation
  for (i in 1:(2^n - 1)) {
    # Convert the current index to binary
    binary_representation <- as.integer(intToBits(i))
    
    # Include elements where the corresponding bit is set to 1
    selected_items <- x[binary_representation == 1, ]
    
    # Calculate the total weight and total value for the selected items
    total_weight <- sum(selected_items$w)
    total_value <- sum(selected_items$v)
    
    # Check if the combination is valid and maximizes value
    if (total_weight <= W && total_value > max_value) {
      max_value <- round(total_value)
      best_elements <- which(binary_representation == 1)
    }
  }
  
  # Return the result as a list
  result <- list(value = max_value, elements = best_elements)
  return(result)
}

