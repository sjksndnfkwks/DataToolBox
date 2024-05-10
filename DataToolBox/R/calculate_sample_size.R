#' Calculate Required Sample Size for Estimating a Proportion
#'
#' This function estimates the required sample size to achieve a desired error in estimating
#' a population proportion with a specified confidence level. The function assumes a simple
#' random sample from a large population.
#'
#' @param p_hat Estimated proportion based on initial data or prior knowledge.
#' @param desired_error Maximum margin of error for the proportion estimate.
#' @param confidence_level Confidence level for the interval estimate, default is 0.95.
#' @return The calculated sample size necessary to achieve the specified error with the given confidence level.
#'
#' @examples
#' required_n = calculate_sample_size(p_hat = 0.5, desired_error = 0.05)
#' print(required_n)
#' @export

calculate_sample_size <- function(p_hat, desired_error, confidence_level = 0.95) {
  # Validate inputs
  if (!is.numeric(p_hat) || !is.numeric(desired_error) || !is.numeric(confidence_level)) {
    stop("All parameters must be numeric.")
  }
  if (p_hat < 0 || p_hat > 1 || desired_error <= 0 || confidence_level <= 0 || confidence_level >= 1) {
    stop("Invalid input values. Ensure p_hat is between 0 and 1, desired_error > 0, and confidence_level between 0 and 1.")
  }

  # Calculate the z-score corresponding to the confidence level
  z <- qnorm((1 + confidence_level) / 2)

  # Calculate the required sample size
  required_sample_size <- (z^2 * p_hat * (1 - p_hat)) / (desired_error^2)

  # Return the ceiling of the calculated sample size to ensure it is an integer
  return(ceiling(required_sample_size))
}
