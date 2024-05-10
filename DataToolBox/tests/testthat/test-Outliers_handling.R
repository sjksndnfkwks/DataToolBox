library(testthat)
library(DataToolBox)
library(ggplot2)
library(gridExtra)

# Test case 1: Invalid input data
test_that("Non-dataframe input throws an error", {
  Invalid_input <- "Non-dataframe data"
  expect_error(Outliers_handling(Invalid_input, 0.5, "target_column"),
               "Error: The input should be in data frame format.")
})

# Test case 2: The target column does not exist in the data
test_that("Nonexistent target column throws an error", {
  input_data <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  expect_error(Outliers_handling(input_data, 0.5, "X"),
               "Error: The target column must be in the data set.")
})

# Test case 3: Non-numeric or non-positive input for threshold
test_that("Non-numeric or non-positive threshold throws an error", {
  input_data <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  expect_error(Outliers_handling(input_data, -999, "a"),
               "Error: Threshold should be an positive value.")
})

# Test case 4: Proper input
test_that("Proper input with expected output", {
  input_data <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  result <- Outliers_handling(input_data, threshold = 2, "a")
  expect_is(result, "data.frame")
})
