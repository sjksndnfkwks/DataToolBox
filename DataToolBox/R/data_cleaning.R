#' Data Cleaning
#'
#' Conduct data cleaning; remove or modify bad data.
#' @param data the data that need cleaning.
#' @param remove_missing whether to remove the missing data in the data set.
#' @param remove_repeating whether to remove the repeated data in the data set.
#' @param fill_NA whether to fill the missing values.
#' @param get_dummies whether to create dummy variables.
#'
#' @return the data after the cleaning procedure.
#'
#' @examples
#' data("SuicideData")
#' data_cleaning(SuicideData)
#'
#' @export

data_cleaning <- function(data, remove_missing=FALSE,
                          remove_repeating=FALSE,
                          fill_NA=FALSE,
                          get_dummies=FALSE) {

  if (!is.data.frame(data)) {
    stop("Error: The input should be in data frame format.")
  }

  # delete the sample data containing NA values if needed
  if (remove_missing) {
    data <- na.omit(data)
  }

  # delete the repeated data if needed
  if (remove_repeating) {
    data <- unique(data)
  }

  # fill the missing values if needed, use mean value to fill as default.
  if (fill_NA) {
    data <- apply(data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
    data <- as.data.frame(data)
  }

  # create dummy variables
  if (get_dummies) {
    for (col in names(data)) {
      if (is.factor(data[[col]]) || is.character(data[[col]])) {
        dummy_vars <- model.matrix(~ data[[col]] - 1)
        colnames(dummy_vars) <- gsub("^data[[col]]", col, colnames(dummy_vars))
        data <- cbind(data, dummy_vars)
        data <- data[, !names(data) %in% col]
      }
    }
  }

  return(data)
}
