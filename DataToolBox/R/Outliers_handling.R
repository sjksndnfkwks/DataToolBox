#' A function to handle outliers in the data set.
#'
#' This function can process the outliers in a data set.
#'
#' @param data the data frame to be processed.
#' @param threshold the threshold to determine whether a value is considered as an outlier.
#' @param target_column the target column as response.
#' @param plot_before whether to plot the original data.
#' @param plot_after whether to plot the processed data.
#'
#' @return the processed data and the plots if needed.
#'
#' @examples
#' data("SuicideData")
#'
#' @export

Outliers_handling <- function(data, threshold = 0.5,
                              target_column,
                              plot_before = TRUE,
                              plot_after = TRUE) {

  if (!is.data.frame(data)) {
    stop("Error: The input should be in data frame format.")
  }

  if (!target_column %in% colnames(data)) {
    stop("Error: The target column must be in the data set.")
  }

  if (!is.numeric(threshold) | threshold <= 0) {
    stop("Error: Threshold should be an positive value.")
  }

  # find the numeric data in the data frame. We need to process numeric data.
  numeric_data <- dplyr::select_if(data, is.numeric)

  # The function to handle outliers.
  handle_numeric <- function(numeric_data) {

    sds <- apply(numeric_data, 2, sd, na.rm = TRUE)
    medians <- apply(data, 2, median, na.rm = TRUE)

    outliers <- abs((numeric_data - medians) / sds) > threshold

    # Replace the outliers with median value.
    for (i in 1:ncol(numeric_data)) {
      numeric_data[outliers[,i], i] <- medians[i]
    }
    return(numeric_data)
  }

  processed_numeric <- handle_numeric(numeric_data)
  processed_data <- processed_numeric

  # plot the original data is needed.
  if (plot_before) {
    plots = list()
    for (col in names(data)) {
      if (col != target_column) {
        plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[target_column]], y = .data[[col]])) +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          ggplot2::labs(title = paste("plot of", col, "vs", target_column)) +
          ggplot2::theme_minimal()
        plots[[col]] <- plot
      }
    }
    num_cols <- ceiling(sqrt(ncol(data) - 1))
    combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol=num_cols))
    print(combined_plot)
  }

  # plot the processed data if needed.
  if (plot_after) {
    plots = list()
    for (col in names(processed_data)) {
      if (col != target_column) {
        plot <- ggplot2::ggplot(processed_data, ggplot2::aes(x = .data[[target_column]], y = .data[[col]])) +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          ggplot2::labs(title = paste("plot of", col, "vs", target_column)) +
          ggplot2::theme_minimal()
        plots[[col]] <- plot
      }
    }
    num_cols <- ceiling(sqrt(ncol(data) - 1))
    combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol=num_cols))
    print(combined_plot)
  }

  return(processed_data)
}
