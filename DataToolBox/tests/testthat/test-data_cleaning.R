library(testthat)
library(DataToolBox)

# Test case 1: Invalid input data
test_that("Non-dataframe input throws an error", {
  Invalid_input <- "Non-dataframe data"
  expect_error(data_cleaning(Invalid_input, ),
               "Error: The input should be in data frame format.")
})

# Test case 2: Handling data containing NA values
test_that("Output data should be no NA values", {
  input_data <- data.frame(a = c(1, 2, NA), b = c(4, 5, 6))
  processed <- data_cleaning(input_data, remove_missing=TRUE)
  expect_is(processed, "data.frame")
  expect_true(all(!is.na(processed)), "No missing values should remain")
})

# Test case 3: Handling data containing repeated rows
test_that("Output data should be no duplicated rows", {
  input_data <- data.frame(a = c(1, 2, 2), b = c(3, 2, 2))
  processed <- data_cleaning(input_data, remove_repeating = TRUE)
  expect_is(processed, "data.frame")
  expect_true(!any(duplicated(processed)), "No duplicated rows should remain")
})

# Test case 4: Filling NA values in data if needed
test_that("NA values replaced by mean values", {
  input_data <- data.frame(a = c(1, 2, NA), b = c(4, 5, 6))
  processed <- data_cleaning(input_data, fill_NA = TRUE)
  expect_is(processed, "data.frame")
  expect_true(all(!is.na(processed)), "Missing values should be filled")
})

# Test case 5: Dummy variables are correctly created
test_that("Creating dummy variables for the given data", {
  input_data <- data.frame(a = c(1, 2, NA), b = c(4, 5, 6))
  processed <- data_cleaning(input_data, get_dummies = TRUE)
  expect_is(processed, "data.frame")
})
