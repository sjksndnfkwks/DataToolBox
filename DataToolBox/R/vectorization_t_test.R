#' Vectorized Statistical Testing on Paired Data Frames
#'
#' Performs specified statistical tests (t-test or chi-square test) on corresponding columns
#' of two data frames. This function is designed to apply the specified test to each pair
#' of columns from the first and second data frame, respectively.
#'
#' @param df1 Data frame containing the first set of variables.
#' @param df2 Data frame containing the second set of variables.
#' @return A list containing the results of the specified test for each pair of columns. Results include the test statistic, degrees of freedom, and p-value.

#'
#' @examples
#' df1 <- data.frame(a = rnorm(100), b = rnorm(100))
#' df2 <- data.frame(a = rnorm(100), b = rnorm(100))
#' vectorization_t_test(df1, df2)
#' @export

vectorization_t_test <- function(df1, df2) {
  if (!is.data.frame(df1) || !is.data.frame(df2)) {
    stop("Invalid input. This function only handles two data frames!")
  }

  # Identify numeric columns in each data frame
  numeric_cols_df1 <- sapply(df1, is.numeric)
  numeric_cols_df2 <- sapply(df2, is.numeric)

  # Filter data frames to only include numeric columns
  df1_numeric <- df1[, numeric_cols_df1]
  df2_numeric <- df2[, numeric_cols_df2]

  # Find common numeric columns in both data frames
  common_cols <- intersect(names(df1_numeric), names(df2_numeric))

  # Initialize list to store test results
  test_results <- list()

  for (col_name in common_cols) {
    column1 <- df1_numeric[[col_name]]
    column2 <- df2_numeric[[col_name]]

    # Ensure columns have more than one unique value for testing
    if (length(unique(column1)) > 1 && length(unique(column2)) > 1) {
      t_result <- t.test(column1, column2)
      test_results[[col_name]] <- list(
        statistic = t_result$statistic,
        df = t_result$parameter,
        p_value = t_result$p.value
      )
    } else {
      test_results[[col_name]] <- NA
      message(sprintf("Some column has constant data. Test skipped."))
    }
  }

  return(test_results)
}

