% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vectorization_t_test.R
\name{vectorization_t_test}
\alias{vectorization_t_test}
\title{Vectorized Statistical Testing on Paired Data Frames}
\usage{
vectorization_t_test(df1, df2)
}
\arguments{
\item{df1}{Data frame containing the first set of variables.}

\item{df2}{Data frame containing the second set of variables.}
}
\value{
A list containing the results of the specified test for each pair of columns. Results include the test statistic, degrees of freedom, and p-value.
}
\description{
Performs specified statistical tests (t-test or chi-square test) on corresponding columns
of two data frames. This function is designed to apply the specified test to each pair
of columns from the first and second data frame, respectively.
}
\examples{
df1 <- data.frame(a = rnorm(100), b = rnorm(100))
df2 <- data.frame(a = rnorm(100), b = rnorm(100))
vectorization_t_test(df1, df2)
}
