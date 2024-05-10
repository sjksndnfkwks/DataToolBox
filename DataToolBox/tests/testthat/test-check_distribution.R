library(testthat)
library(DataToolBox)

# Test case 1: Numeric data, normal distribution
test_that("check_distribution function behaves as expected",{
  data <- rnorm(100)
  expect_equal(check_distribution(data, "normal"), "By Kolmogorov-Smirnov test, your data follows normal distribution.")
})

# Test case 2: Wrong distribution type
test_that("check wrong input distribution type",{
  data <- rnorm(100)
  expect_equal(check_distribution(data, "aaaaaa"), "Wrong distribution type. Please choose distribution from 'normal', 'uniform', 'exponential', 'poisson', 'binomial'.")
})

# Test case 3: Wrong input data
test_that("check wrong input data",{
  data <- "aaaaaa"
  expect_equal(check_distribution(data, "normal"), "This data cannot be tested because it is not numeric. Please try others.")
})
