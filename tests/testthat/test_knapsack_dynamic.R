context("knapsack_dynamic")

test_that("Correct object is returned", {
  expect_silent(dk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(dk, c("max_value", "selected_items"))
})

test_that("Function rejects erroneous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  dk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(dk$max_value), 16770)
  # Modify the expectation for selected_items based on the expected output format
  
  # Test additional cases
  dk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(dk$max_value), 16770)
  # Modify the expectation for selected_items
  
  dk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(dk$max_value), 15428)
  # Modify the expectation for selected_items
  
  dk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(dk$max_value), 15428)
  # Modify the expectation for selected_items
  
  st <- system.time(dk <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})
