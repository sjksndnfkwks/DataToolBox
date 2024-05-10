library(testthat)
library(DataToolBox)

# Test 1: Input validation
test_that("Function handles non-dataframe inputs", {
  non_dataframe_input <- "Not a dataframe"
  expect_error(backward_selection_regression(non_dataframe_input, "response", 2),
               "Your input format is not correct. The data should be in data frame format. Please change.")
})

# Test 2: Response variable existence
test_that("Function stops if response variable isn't in dataset", {
  data <- data.frame(a = 1:4, b = 2:5)
  expect_error(backward_selection_regression(data, "c", 1),
               "The response variable does not exist in the dataset.")
})

# Test 3: Proper number of regressors
test_that("Function reduces regressors to expected number", {
  sampledata = as.data.frame(read.csv("https://raw.githubusercontent.com/sjksndnfkwks/HypoRegVis_dataset/main/master(1).csv"))
  result = backward_selection_regression(sampledata, "suicides.100k.pop", 3)
  expect_length(result, 3)
})

# Test 4: At least one response variable and one predictor variable
test_that("Function stops if there do not have at least one response variable and one predictor variable", {
  data <- data.frame(a = c(1,2,3))
  expect_error(backward_selection_regression(data, "a", 1),
               "The data must contain at least one response variable and one predictor variable.")
})

# Test 5: The expected number of regressors does not exceed the number of available predictors
test_that("Function stops if the expected number of regressors exceed the number of available predictors", {
  data <- data.frame(y = 1:10, x1 = rnorm(10), x2 = rnorm(10), x3 = rnorm(10))
  expect_error(backward_selection_regression(data, "y", 10),
               "The expected number of regressors exceeds the number of available predictors.")
})
