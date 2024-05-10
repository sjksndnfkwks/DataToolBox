library(testthat)
library(DataToolBox)

# Test 1: Correct Input and Expected Output
test_that("Correct input returns expected output", {
  observed <- matrix(c(10, 10, 20, 20), nrow = 2)
  alpha <- 0.05
  result <- calculate_contingency_table(observed, alpha)
  expect_is(result, "list")
  expect_equal(length(result), 3)
  expect_equal(names(result), c("chi_square_stat", "p_val", "decision"))
})

# Test 2: Non-matrix input should throw an error
test_that("Non-matrix input throws an error", {
  observed <- c(10, 20, 30, 40)  # Not a matrix
  alpha <- 0.05
  expect_error(calculate_contingency_table(observed, alpha),
               "Error: Input 'observed' must be a matrix.")
})

# Test 3: Non-numeric matrix input should throw an error
test_that("Non-numeric matrix input throws an error", {
  observed <- matrix(c("a", "b", "c", "d"), nrow = 2)
  alpha <- 0.05
  expect_error(calculate_contingency_table(observed, alpha),
               "Error: All elements in 'observed' must be numeric.")
})

# Test 4: Matrix with negative numbers should throw an error
test_that("Matrix with negative numbers throws an error", {
  observed <- matrix(c(-1, 5, 10, 15), nrow = 2)
  alpha <- 0.05
  expect_error(calculate_contingency_table(observed, alpha),
               "Error: All elements in 'observed' must be non-negative.")
})

# Test 5: Incorrect alpha value should throw an error
test_that("Alpha value out of bounds throws an error", {
  observed <- matrix(c(10, 20, 20, 40), nrow = 2)
  alpha <- 1.5  # Invalid alpha
  expect_error(calculate_contingency_table(observed, alpha),
               "Error: 'alpha' must be a numeric value between 0 and 1.")
})
