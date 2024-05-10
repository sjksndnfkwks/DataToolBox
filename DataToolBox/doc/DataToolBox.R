## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
install.packages("/path/to/DataToolBox_0.9.0.tar.gz", repos = NULL, type = "source")

## -----------------------------------------------------------------------------
library(DataToolBox)

## -----------------------------------------------------------------------------
data("SuicideData")
head(SuicideData,100)

## ----eval = FALSE-------------------------------------------------------------
#  plot_data_analysis(SuicideData, hist = TRUE, pair = TRUE, hist_col = "lightblue", pair_col = "darkgreen")

## -----------------------------------------------------------------------------
check_distribution(SuicideData$HDI.for.year, "uniform")
check_distribution(SuicideData$gdp_per_capita...., "exponential")
check_distribution(SuicideData$suicides.100k.pop, "normal")

## -----------------------------------------------------------------------------
clean_SuicideData <- data_cleaning(SuicideData, TRUE, TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  clean_SuicideData <- Outliers_handling(SuicideData, threshold = 0.5, "suicides_no")

## -----------------------------------------------------------------------------
forward_selection_regression(clean_SuicideData, "suicides.100k.pop", 4, summary = TRUE)

## -----------------------------------------------------------------------------
backward_selection_regression(clean_SuicideData, "suicides.100k.pop", 4, summary = TRUE)

## -----------------------------------------------------------------------------
library(dplyr)
library(tidyr)
SuicideData_Arg = SuicideData %>%
  filter(country == "Argentina")
SuicideData_Arg = SuicideData_Arg %>%
  select(sex, age, suicides_no) %>%
  spread(age, suicides_no)

SuicideData_Arg_mat <- as.matrix(SuicideData_Arg)
SuicideData_Arg_mat = SuicideData_Arg_mat[, -1]
SuicideData_Arg_mat = matrix(as.numeric(SuicideData_Arg_mat), nrow = nrow(SuicideData_Arg_mat))
rownames(SuicideData_Arg_mat) = c("female", "male")
colnames(SuicideData_Arg_mat) = c("15-24 years", "25-34 years", "35-54 years",
                                 "5-14 years", "55-74 years", "75+ years")

#Assume that H0: The age of suicides_no in Argentina has no difference from man to women.
calculate_contingency_table(SuicideData_Arg_mat, alpha = 0.05)

## -----------------------------------------------------------------------------
#Suppose you want to estimate the proportion of suicides in a specific demographic group (e.g., females aged 15-24) within a certain country. You would first need to calculate an initial estimate of the proportion of suicides in this group (p_hat), which could be based on previous data or preliminary analysis.

SuicideData_Arg = SuicideData %>%
  filter(country == "Argentina")
p_hat = sum(SuicideData_Arg$sex == "female" & SuicideData_Arg$age == "15-24 years")/sum(SuicideData_Arg$sex == "female")
calculate_sample_size(p_hat, desired_error = 0.05, confidence_level = 0.95) # Should be 214

#So we can know that at least 214 individuals should be surveyed to estimated within a 5% error margin with 95% confidence.

## -----------------------------------------------------------------------------
SuicideData_Arg = SuicideData %>%
  filter(country == "Argentina")
SuicideData_Arm = SuicideData %>%
  filter(country == "Armenia")

vectorization_t_test(SuicideData_Arg, SuicideData_Arm)

#The results implies that 
#suicides_no:
#The t-test statistic is 3.611882.
#With 11.01792 degrees of freedom, this yields a p-value of 0.004073629.
#The p-value is less than 0.05, suggesting that there is a statistically significant difference in the number of suicides between the two datasets.
#population:
#  The t-test statistic is 8.46542.
#With 11.15149 degrees of freedom, the p-value is very small (3.465129e-06).
#This extremely low p-value indicates a highly significant difference in population between the two datasets.
#suicides.100k.pop:
#  The t-test statistic is 2.356893.
#With 13.02052 degrees of freedom, the p-value is 0.03474843.
#The p-value is less than 0.05, indicating a statistically significant difference in suicide rates per 100k population between the datasets.

