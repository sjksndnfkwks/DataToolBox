#' Chi-square Test for Independence from Contingency Table
#'
#' Performs a chi-square test of independence between two categorical variables
#' represented in a contingency table (matrix of observed counts). This function
#' computes the chi-square statistic, degrees of freedom, and p-value to evaluate
#' whether there is a significant association between the two variables.
#'
#' @param observed A matrix where each element represents the count of observations for the combination of categories from the two variables.
#' @param alpha The significance level used to determine the critical value for the hypothesis test, default to be 0.05 (typically set between 0.01 and 0.05).
#' @return A list containing the chi-square statistic, degrees of freedom, p-value, and the decision to either reject or fail to reject the null hypothesis of independence.
#'
#' @examples
#' observed = matrix(c(10, 20, 20, 40), nrow = 2, byrow = TRUE)
#' result = calculate_contingency_table(observed, alpha = 0.05)
#' print(result)
#' @export



calculate_contingency_table <- function(observed, alpha = 0.05) {
  # Check if the input is a matrix
  if (!is.matrix(observed)) {
    stop("Error: Input 'observed' must be a matrix.")
  }

  # Check if the matrix contains only numeric values
  if (!all(is.numeric(observed))) {
    stop("Error: All elements in 'observed' must be numeric.")
  }

  # Check for non-negative values in the matrix
  if (any(observed < 0)) {
    stop("Error: All elements in 'observed' must be non-negative.")
  }

  # Check if alpha is a numeric value between 0 and 1
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Error: 'alpha' must be a numeric value between 0 and 1.")
  }

  # Calculate row and column totals
  row_totals <- apply(observed, 1, sum)
  col_totals <- apply(observed, 2, sum)
  total <- sum(observed)

  # Calculate expected frequencies
  expected <- outer(row_totals, col_totals) / total

  # Calculate chi-square statistic
  chi_square_stat <- sum((observed - expected)^2 / expected)

  # Calculate degrees of freedom
  df <- (length(row_totals) - 1) * (length(col_totals) - 1)

  # Calculate p-value using chi-square distribution
  p_val <- pchisq(chi_square_stat, df, lower.tail = FALSE)

  # Decide based on the p-value
  if(p_val < alpha) {
    result <- "Reject the null hypothesis."
  } else {
    result <- "Fail to reject the null hypothesis."
  }

  # Return chi-square statistic, p-value, and decision
  return(list(chi_square_stat = chi_square_stat, p_val = p_val, decision = result))
}
