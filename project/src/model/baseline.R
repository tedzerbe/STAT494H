# Front Matter ---------------------------------------------------------------------
library(tidyverse)
library(tidymodels)

# Load data
data <- read.csv("project/volume/data/processed/final.csv")

# Function to compute weighted mean squared error
weighted_mse_impl <- function(truth, estimate, case_weights = NULL) {
  if (is.null(case_weights)) {
    mean((truth - estimate) ^ 2)
  } else {
    # Ensure weights are normalized
    case_weights <- case_weights / sum(case_weights)
    sum((truth - estimate) ^ 2 * case_weights)
  }
}

# Vectorized function to compute weighted RMSE, handling NA values and using case weights
weighted_rmse_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)
  
  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)
    
    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }
  
  # Compute the square root of the weighted MSE
  sqrt(weighted_mse_impl(truth, estimate, case_weights = case_weights))
}

# S3 generic function for RMSE
weighted_rmse <- function(data, ...) {
  UseMethod("weighted_rmse")
}

# Create a new numeric metric for weighted RMSE
weighted_rmse <- new_numeric_metric(weighted_rmse, direction = "minimize")

# Method to calculate weighted RMSE for data.frame input
weighted_rmse.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  numeric_metric_summarizer(
    name = "weighted_rmse",
    fn = weighted_rmse_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

num_seeds <- 100



# Null Model ---------------------------------------------------------------------
test_errors <- data.frame()

for (i in 1:num_seeds) {
  set.seed(i)
  
  # Split the data with random seed
  data_split <- initial_split(data, prop = 0.8)
  train <- training(data_split)
  test <- testing(data_split)
  folds <- vfold_cv(train, v = 10)
  
  null_pred <- 100
  
  test_preds <- test %>%
    mutate(.pred = null_pred) %>%
    select(PlayerID, Name, Season_future, Start_IP_future, ERA_Minus_future, .pred, Start_IP_past1)
  
  # Calculate test error
  test_error <- weighted_rmse.data.frame(
    data = test_preds,
    truth = ERA_Minus_future,
    estimate = .pred,
    na_rm = TRUE,
    case_weights = Start_IP_future
  )
  
  # Save testing error
  test_errors <- rbind(test_errors, test_error)
}

# Save testing errors and variable importances
write.csv(test_errors, "project/volume/models/null_errors.csv")



# Naive Model ---------------------------------------------------------------------
test_errors <- data.frame()

for (i in 1:num_seeds) {
  set.seed(i)
  
  # Split the data with random seed
  data_split <- initial_split(data, prop = 0.8)
  train <- training(data_split)
  test <- testing(data_split)
  folds <- vfold_cv(train, v = 10)
  
  test_preds <- test %>%
    mutate(.pred = ifelse(is.na(xFIP_Minus_past1) | Start_IP_past1 < 60, null_pred, xFIP_Minus_past1)) %>%
    select(PlayerID, Name, Season_future, Start_IP_future, ERA_Minus_future, .pred, Start_IP_past1)
  
  # Calculate test error
  test_error <- weighted_rmse.data.frame(
    data = test_preds,
    truth = ERA_Minus_future,
    estimate = .pred,
    na_rm = TRUE,
    case_weights = Start_IP_future
  )
  
  # Save testing error
  test_errors <- rbind(test_errors, test_error)
}

# Save testing errors and variable importances
write.csv(test_errors, "project/volume/models/naive_errors.csv")
