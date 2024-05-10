#' Function to backward select variables and do regression based on selected variables.
#'
#' This function takes a data set, a response variable and expected number of regressors as input
#' and returns the selected variable based by backward selection and AIC.
#' Also returns the summary of corresponding regression model if you set parameter summary TRUE.
#'
#' @param data A data frame type data set for regression and variable selection
#' @param response A response variable for your regression in character format, eg:"xxx"
#' @param expected_number_of_regressor Expected number of regressors
#' @param summary whether do a summary for the regression model based on the selected variables
#' @return The selected variables, summary of such regression model if you set the parameter summary TRUE
#' @examples
#' # The following sample data set includes socio-economic information with suicide rates by country in 2011.
#' data("SuicideData")
#' backward_selection_regression(SuicideData, "suicides.100k.pop", 3)
#' backward_selection_regression(SuicideData, "suicides.100k.pop", 4)
#' backward_selection_regression(SuicideData, "suicides.100k.pop", 3, summary = TRUE)
#'
#' @export

backward_selection_regression = function(data, response, expected_number_of_regressor, summary = FALSE){
  if (!is.data.frame(data) == TRUE){
    stop("Your input format is not correct. The data should be in data frame format. Please change.")
  }
  data = dplyr::select_if(data, is.numeric) # we only focus on numeric

  if (!response %in% colnames(data)) {
    stop("The response variable does not exist in the dataset.")
  }

  if (ncol(data) < 2) {
    stop("The data must contain at least one response variable and one predictor variable.")
  }

  if (expected_number_of_regressor > length(colnames(data)) - 1) {
    stop("The expected number of regressors exceeds the number of available predictors.")
  }
  regressor_set = colnames(data)[colnames(data) != response]
  selected_regressors = regressor_set
  best_aic = Inf

  # backward selection of predictor variables
  while (length(selected_regressors) > expected_number_of_regressor) {
    candidate_aic = numeric(length(selected_regressors))
    for (i in seq_along(selected_regressors)) {
      predictors = selected_regressors[-i]
      current_formula = as.formula(paste(response, "~", paste(predictors, collapse = "+")))
      model = lm(formula = current_formula, data = data)
      candidate_aic[i] = AIC(model)
    }

    # we want to lowest the aic
    best_candidate_index = which.min(candidate_aic)
    best_candidate_aic = candidate_aic[best_candidate_index]

    # If the AIC value of the best candidate model is lower than the current best, the remove it out the selected set
    if (best_candidate_aic < best_aic) {
      selected_regressors = selected_regressors[-best_candidate_index]
      best_aic = best_candidate_aic
    } else {
      break  #no better
    }
  }

  selected_regressors = selected_regressors[1:expected_number_of_regressor]
  final_formula = as.formula(paste(response, "~", paste(selected_regressors, collapse = "+")))
  # If the summary parameter is TRUE, return the summary of the final model
  if (summary) {
    final_model = lm(formula = final_formula, data = data)
    print(selected_regressors)
    return(summary(final_model))
  } else {
    return(selected_regressors)
  }
}
