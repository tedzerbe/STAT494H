# Front Matter ---------------------------------------------------------------------
set.seed(34) # Setting a seed for reproducibility

# Load libraries
library(tidyverse)
library(tidymodels)
library(vip)
library(rlang)

# Load data
data <- read.csv("project/volume/data/processed/final.csv")

# Remove past arsenal grades as features
data <- data %>% select(-botStf_past1, -Stuff_Plus_past1, -Pitching_Plus_past1)

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

# Predefine seeds for splitting the data
seeds <- c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110)

# Initialize a vector to store test errors
test_errors <- data.frame()
vips <- data.frame()

for(i in seq_along(seeds)) {
  set.seed(seeds[i])
  
  # Split the data with random seed
  data_split <- initial_split(data, prop = 0.8)
  train <- training(data_split)
  test <- testing(data_split)
  folds <- vfold_cv(train, v = 10)
  
  # Create model specification
  spec <- boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(), sample_size = tune()) %>%
    set_mode("regression") %>%
    set_engine("xgboost")
  
  # Create model recipe for preprocessing
  rec <- recipe(ERA_Minus_future ~ ., data = train) %>%
    update_role(PlayerID, Name, Season_future, Start_IP_future, new_role = "ID")
  
  # Create workflow object
  wf <- workflow() %>%
    add_model(spec) %>%
    add_recipe(rec)
  
  # Hyperparameter tune and validate models using Bayesian optimization
  tune_res <- wf %>%
    tune_bayes(resamples = folds,
               initial = 10,
               iter = 100,
               control = control_bayes(verbose = TRUE, no_improve = 10, seed = 34),
               metrics = metric_set(weighted_rmse))
  
  # Select best hyperparameter setting
  best_params <- select_best(tune_res, "weighted_rmse")
  
  # Fit the final model
  set.seed(34)
  final_model <- wf %>%
    finalize_workflow(best_params) %>%
    fit(data = train)
  
  # Save the model
  model_name <- paste("model_without_", seeds[i], ".rds", sep = "")
  saveRDS(final_model, paste0("project/volume/models/", model_name))
  
  # Generate predictions on test set
  test_preds <- predict(final_model, test) %>%
    bind_cols(test)
  
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
  
  # Generate feature importance plot
  vip <- final_model %>% 
    extract_fit_parsnip() %>%
    vip(num_features = 100)
  vip <- vip[[1]]
  vip <- pivot_wider(data = vip, names_from = Variable, values_from = Importance)
  
  # Save feature importance scores
  vips <- rbind(vips, vip)
}

# Save testing errors and variable importances
write.csv(test_errors, "project/volume/models/model_without_errors.csv")
write.csv(vips, "project/volume/plots/model_without_vips.csv")