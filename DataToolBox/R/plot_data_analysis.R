#' Plot Data Analysis
#'
#' Plots histograms and pairwise relationships for numeric variables in a data frame.
#'
#' @param data A data frame containing numeric variables to be analyzed.
#' @param hist Logical. If \code{TRUE}, histograms of numeric variables will be plotted. Default is \code{TRUE}.
#' @param pair Logical. If \code{TRUE} and more than one numeric variable exists, pairwise relationships will be plotted. Default is \code{TRUE}.
#' @param hist_col Color specification for histograms, default to be lightblue. If provided, the same color will be used for all histograms.
#' @param pair_col Color specification for pairwise plots, default to be darkgreen. If provided, the same color will be used for all pairwise plots.
#' @return NULL
#'
#' @examples
#' data <- data.frame(
#'   Age = c(25, 30, 35, 40, 45),
#'   Height = c(175, 180, 165, 170, 160),
#'   Weight = c(70, 80, 75, 65, 60),
#'   Gender = as.factor(c("Male", "Female", "Male", "Female", "Male")),
#'   Income = c(50000, 60000, 55000, 58000, 62000)
#' )
#' plot_data_analysis(data)
#'
#'
#' @export
#'
plot_data_analysis <- function(data, hist = TRUE, pair = TRUE, hist_col = "lightblue", pair_col = "darkgreen") {
  # Check if the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  # Filter out the numeric columns
  numeric_data <- data[sapply(data, is.numeric)]

  # Check if there are numeric columns
  if (ncol(numeric_data) == 0) {
    stop("No numeric columns in the data frame")
  }

  if (hist) {
    # Define the layout for histograms
    num_numeric <- ncol(numeric_data)
    layout_rows <- ceiling(sqrt(num_numeric))
    layout_cols <- ceiling(num_numeric / layout_rows)

    # Set the layout parameters for histograms
    par(mfrow = c(layout_rows, layout_cols))

    # Loop through each numeric column to plot histograms
    for (i in names(numeric_data)) {
      hist(numeric_data[[i]], main=paste("Histogram of", i), xlab=i, col = hist_col)
    }

    # Reset to default single plot layout
    par(mfrow = c(1, 1))
  }

  if (pair && ncol(numeric_data) > 1) {
    # Plotting pairwise relationships with pairs() if more than one numeric variable exists
    pairs(numeric_data, main = "Pairwise Relationships", pch = 20, col = pair_col)
  } else if (pair) {
    cat("Not enough variables for a pairs plot.")
  }
}
