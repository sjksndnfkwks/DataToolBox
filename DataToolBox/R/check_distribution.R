#' Function to check distribution
#'
#' This function takes a data and an expected distribution as input and returns
#' whether the data follows expected distribution.
#'
#' @param data data frame
#' @param distribution The distribution you expected
#' @param detail Set TRUE to show the details of the test
#' @param size Number of trials
#' @param prob The probability for the binomial distribution
#' @return Whether the data follows expected distribution
#' @examples
#' data("SuicideData")
#' check_distribution(SuicideData$suicides.100k.pop, "normal")
#' check_distribution(SuicideData$suicides.100k.pop, "normal", detail = TRUE)
#' set.seed(1)
#' check_distribution(runif(1000),"uniform", detail = TRUE)
#' check_distribution(rpois(1000,2), "poisson", detail =TRUE)
#' check_distribution(rbinom(1000,100,0.5), "binomial", detail =TRUE, size = 100, prob = 0.5)
#'
#' @export

check_distribution = function(data, distribution, detail = FALSE, size = 1000, prob = 0.5) {
  if (!is.numeric(data)) {
    return("This data cannot be tested because it is not numeric. Please try others.")
  }
  # Generate theoretical distribution
  if (distribution == "normal") {
    theoretical_distribution = rnorm(length(data), mean = mean(data,na.rm = TRUE), sd = sd(data,na.rm = TRUE))
  } else if (distribution == "uniform") {
    theoretical_distribution = runif(length(data), min = min(data,na.rm = TRUE), max = max(data,na.rm = TRUE))
  } else if (distribution == "exponential") {
    theoretical_distribution = rexp(length(data), rate = 1/ mean(data,na.rm = TRUE))
  } else if (distribution == "poisson") {
    theoretical_distribution = rpois(length(data), lambda = mean(data,na.rm = TRUE))
  } else if (distribution == "binomial") {
    theoretical_distribution = rbinom(length(data), size, prob)
  } else {
    return("Wrong distribution type. Please choose distribution from 'normal', 'uniform', 'exponential', 'poisson', 'binomial'.")
  }
  # Do the Kolmogorov-Smirnov Test
  ks_result = ks.test(data, theoretical_distribution, exact = TRUE)
  if (ks_result$p.value < 0.05) {
    return(paste("By Kolmogorov-Smirnov test, your data does NOT follow",
                 distribution, "distribution."))
  } else {
    return(paste("By Kolmogorov-Smirnov test, your data follows", distribution,
                 "distribution."))
  }

  if(detail == TRUE){
    return(ks_result)
  }
  detail = FALSE
}
