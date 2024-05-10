library(testthat)
library(DataToolBox)

# Test 1: Correct Input and Expected Output
test_that("Correct input returns expected output", {
  # Create a sample data frame with numeric variables
  data <- data.frame(
    Age = c(25, 30, 35, 40, 45),
    Height = c(175, 180, 165, 170, 160),
    Weight = c(70, 80, 75, 65, 60),
    Gender = as.factor(c("Male", "Female", "Male", "Female", "Male")),
    Income = c(50000, 60000, 55000, 58000, 62000)
  )

  # Test that the function runs without errors
  expect_no_error(plot_data_analysis(data))

})

# Test 2: Non-data frame input should throw an error
test_that("Non-data frame input throws an error", {
  data <- c(1, 2, 3, 4, 5)  # Not a data frame
  expect_error(plot_data_analysis(data), "Input must be a data frame")
})

# Test 3: Data frame with no numeric columns should throw an error
test_that("Data frame with no numeric columns throws an error", {
  data <- data.frame(Gender = as.factor(c("Male", "Female", "Male", "Female", "Male")))
  expect_error(plot_data_analysis(data), "No numeric columns in the data frame")
})
