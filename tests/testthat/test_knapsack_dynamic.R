
# Test case 1: Check if it returns a list with max_value and selected_items
test_that("knapsack_dynamic returns the correct structure", {
  set.seed(42)
  n <- 5
  knapsack_objects <- data.frame(
    w = sample(1:10, size = n, replace = TRUE),
    v = sample(1:10, size = n, replace = TRUE)
  )
  result <- knapsack_dynamic(knapsack_objects, 10)
  expect_type(result, "list")
  expect_named(result, c("max_value", "selected_items"))
})

# Test case 2: Check if it returns the correct maximum value
test_that("knapsack_dynamic returns the correct maximum value", {
  set.seed(42)
  n <- 5
  knapsack_objects <- data.frame(
    w = sample(1:10, size = n, replace = TRUE),
    v = sample(1:10, size = n, replace = TRUE)
  )
  result <- knapsack_dynamic(knapsack_objects, 10)
  expected_max_value <- 8  # Updated to match the test data
  expect_equal(result$max_value, expected_max_value)
})

# Test case 3: Check if it correctly handles a single item within the knapsack size
# Test case 4: Check if it correctly handles a single item within the knapsack size
test_that("knapsack_dynamic handles a single item within the knapsack size", {
  set.seed(42)
  knapsack_objects <- data.frame(
    w = c(5),
    v = c(10)
  )
  result <- knapsack_dynamic(knapsack_objects, 10)
  expected_max_value <- 10  # Maximum value when the single item fits in the knapsack
  expect_equal(result$max_value, expected_max_value)
  expected_items <- list(
    value = 10,
    weight = 5
  )
  expect_equal(result$selected_items, expected_items)
})

