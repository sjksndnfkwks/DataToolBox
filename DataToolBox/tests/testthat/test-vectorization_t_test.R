library(testthat)
library(dplyr)

df1 <- data.frame(a = rnorm(100), b = rnorm(100))
df2 <- data.frame(a = rnorm(100), b = rnorm(100))

test_that("Function returns correct results for valid inputs", {
  # Test the function
  result <- vectorization_t_test(df1, df2)

  # Check if the result is a list
  expect_is(result, "list")

  # Check the length of the result list
  expect_length(result, 2)  # Assuming both data frames have two numeric columns

  # Check if each element in the result list is a list with correct components
  expect_true(all(sapply(result, function(x) is.list(x) &&
                           all(c("statistic", "df", "p_value") %in% names(x)))))
})

test_that("Function handles invalid inputs correctly", {
  # Test with non-data frame inputs
  expect_error(vectorization_t_test(1, df2), "Invalid input")
  expect_error(vectorization_t_test(df1, "test"), "Invalid input")

  # Test with data frames containing constant columns
  df3 <- data.frame(a = rep(1, 100), b = rep(2, 100))
  expect_message(vectorization_t_test(df1, df3), "Some column has constant data. Test skipped.")
})


