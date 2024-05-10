library(testthat)
library(DataToolBox)

# Test for correct calculation
test_that("correctly calculates sample size", {
  expect_equal(
    calculate_sample_size(p_hat = 0.5, desired_error = 0.05, confidence_level = 0.95),
    385
  )
})

# Test for handling invalid p_hat values
test_that("errors on invalid p_hat values", {
  expect_error(calculate_sample_size(p_hat = -0.1, desired_error = 0.05))
  expect_error(calculate_sample_size(p_hat = 1.1, desired_error = 0.05))
})

# Test for handling non-numeric input
test_that("errors on non-numeric input", {
  expect_error(calculate_sample_size(p_hat = "half", desired_error = 0.05))
  expect_error(calculate_sample_size(p_hat = 0.5, desired_error = "small"))
})

# Test for handling invalid desired_error values
test_that("errors on invalid desired_error values", {
  expect_error(calculate_sample_size(p_hat = 0.5, desired_error = 0))
  expect_error(calculate_sample_size(p_hat = 0.5, desired_error = -0.05))
})

# Test for handling invalid confidence_level values
test_that("errors on invalid confidence_level values", {
  expect_error(calculate_sample_size(p_hat = 0.5, desired_error = 0.05, confidence_level = 0))
  expect_error(calculate_sample_size(p_hat = 0.5, desired_error = 0.05, confidence_level = 1.1))
})

# Test for non-standard but valid inputs
test_that("handles edge valid values", {
  expect_equal(
    calculate_sample_size(p_hat = 0.01, desired_error = 0.01, confidence_level = 0.99),
    ceiling((qnorm((1 + 0.99) / 2)^2 * 0.01 * (1 - 0.01) / (0.01^2)))
  )
  expect_equal(
    calculate_sample_size(p_hat = 0.99, desired_error = 0.01, confidence_level = 0.99),
    ceiling((qnorm((1 + 0.99) / 2)^2 * 0.99 * (1 - 0.99) / (0.01^2)))
  )
})
