# Load the required library
library(testthat)

# Load the MNLMIXTE package
library(mnlmixte)

# Sample data
data("students")  # Assuming 'students' dataset is included in your package
X <- students[, !(colnames(students) %in% "Target")]
y <- as.factor(students$Target)

# Initialize the model
test_that("Initialization works", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 1000, regularization = 0.01)
  expect_equal(model$learning_rate, 0.01)
  expect_equal(model$epochs, 1000)
  expect_equal(model$regularization, 0.01)
})

# Dependency check
test_that("Dependency check works", {
  model <- MNLMIXTE$new()
  expect_no_error(model$dependencies())
})

# Preprocessing
test_that("Preprocessing works", {
  model <- MNLMIXTE$new()
  preprocessed <- model$preprocess_data(X)
  expect_type(preprocessed, "double")
  expect_true(is.matrix(preprocessed))
})

# Fit model
test_that("Model fitting works", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500)
  expect_no_error(model$fit(X, y, variable_selection = FALSE, use_parallel = FALSE))
  expect_false(is.null(model$coefficients))
})

# Predictions
test_that("Predictions work", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500)
  model$fit(X, y, variable_selection = FALSE, use_parallel = FALSE)
  predictions <- model$predict(X)
  expect_type(predictions, "integer")
  expect_equal(length(predictions), nrow(X))
})

# Predict probabilities
test_that("Prediction probabilities work", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500)
  model$fit(X, y, variable_selection = FALSE, use_parallel = FALSE)
  probabilities <- model$predict_proba(X)
  expect_type(probabilities, "double")
  expect_equal(nrow(probabilities), nrow(X))
  expect_equal(ncol(probabilities), length(unique(y)))
})

# Evaluation
test_that("Evaluation works", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500)
  model$fit(X, y, variable_selection = FALSE, use_parallel = FALSE)
  results <- model$evaluate(X, y)
  expect_type(results, "list")
  expect_true(all(c("precision", "recall", "f1_score", "auc") %in% colnames(results)))
})

# Feature importance
test_that("Feature importance works", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500)
  model$fit(X, y, variable_selection = FALSE, use_parallel = FALSE)
  importance <- model$var_importance()
  expect_type(importance, "double")
  expect_true(length(importance) > 0)
})

# Variable selection
test_that("Variable selection works", {
  model <- MNLMIXTE$new()
  selected_features <- model$var_select(X, y)
  expect_type(selected_features, "character")
  expect_true(length(selected_features) > 0)
})

# Plotting
test_that("Loss plot works", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500)
  model$fit(X, y, variable_selection = FALSE, use_parallel = FALSE)
  expect_no_error(model$plot_loss())
})

test_that("Feature importance plot works", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500)
  model$fit(X, y, variable_selection = FALSE, use_parallel = FALSE)
  expect_no_error(model$plot_importance())
})

# Export to PMML
test_that("PMML export works", {
  model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500)
  model$fit(X, y, variable_selection = FALSE, use_parallel = FALSE)
  expect_no_error(model$to_pmml("test_model.pmml"))
  expect_true(file.exists("test_model.pmml"))
  unlink("test_model.pmml")  # Clean up
})
