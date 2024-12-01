#' Multinomial Logistic Regression Model for Mixed Data
#'
#' The `MNLMIXTE` R6 class implements multinomial logistic regression for mixed datasets
#' (categorical and numerical features). It supports variable selection, parallel processing,
#' and model evaluation. This class is designed to preprocess input data, fit a logistic regression
#' model, predict outcomes, and evaluate performance.
#'
#' @field learning_rate Numeric. Learning rate for gradient descent.
#' @field epochs Integer. Number of training epochs (iterations).
#' @field coefficients Matrix. Coefficients of the trained model.
#' @field classes Character vector. Unique class labels in the target variable.
#' @field class_frequencies Numeric vector. Proportions of each class in the training dataset.
#' @field feature_names Character vector. Names of features used during training.
#' @field regularization Numeric. Regularization strength to control overfitting.
#' @field epsilon Numeric. Small constant to avoid division by zero.
#' @field loss_history Numeric vector. Stores the evolution of the loss function during training.
#' @field loss_function Character. Specifies the loss function ("logloss" or "hinge").
#'
#' @examples
#' \dontrun{
#' model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 1000, regularization = 0.01)
#' data(students)
#' X <- students[, !(colnames(students) %in% "Target")]
#' y <- as.factor(students$Target)
#' model$fit(X, y, use_parallel = FALSE)
#' predictions <- model$predict(X)
#' probabilities <- model$predict_proba(X)
#' print(model$evaluate(X, y))
#'}
#' @export
MNLMIXTE <- R6::R6Class(
  "MNLMIXTE",

  public = list(
    # Public fields
    learning_rate = NULL,
    epochs = NULL,
    coefficients = NULL,
    classes = NULL,
    class_frequencies = NULL,
    feature_names = NULL,
    regularization = NULL,
    epsilon = NULL,
    loss_history = NULL,
    loss_function = NULL,

    #' @description
    #' Initialize the multinomial logistic regression model with specified parameters.
    #'
    #' @param learning_rate Numeric. Learning rate for gradient descent. Default: 0.01.
    #' @param epochs Integer. Number of training epochs. Default: 1000.
    #' @param regularization Numeric. Regularization strength. Default: 0.01.
    #' @param epsilon Numeric. Small constant to avoid division by zero. Default: 1e-8.
    #' @param loss_function Character. Specifies the loss function ("logloss" or "hinge"). Default: "logloss".
    #' @return An initialized `MNLMIXTE` object.
    initialize = function(learning_rate = 0.01, epochs = 1000, regularization = 0.01,
                          epsilon = 1e-8, loss_function = "logloss") {
      # Initialize fields
      self$learning_rate <- learning_rate
      self$epochs <- epochs
      self$regularization <- regularization
      self$epsilon <- epsilon
      self$coefficients <- NULL
      self$loss_history <- c()
      self$loss_function <- loss_function
    },

    # Methods to expose utility functions

    #' Check and Install Missing Dependencies
    dependencies = function() {
      dependencies()
    },

    #' Preprocess Data
    #'
    #' @param X A matrix or data frame of features.
    #' @return A preprocessed matrix.
    preprocess_data = function(X) {
      preprocess_data(X)
    },

    #' Prepare Data for Prediction
    #'
    #' @param X A matrix or data frame of features.
    #' @return A matrix prepared for prediction.
    prepare_for_prediction = function(X) {
      prepare_for_prediction(X, self$feature_names)
    },

    #' Compute Variable Importance
    var_importance = function() {
      var_importance(self)
    },

    #' Softmax Function
    #'
    #' Applies the softmax function to a matrix of scores.
    #'
    #' @param z A numeric matrix of scores.
    #' @return A matrix of probabilities after applying the softmax transformation.
    softmax = function(z) {
      softmax(z)
    },

    #' Export Model to PMML
    #'
    #' @param file_path Character. File path for the PMML model.
    #' @return None. Saves the PMML model.
    to_pmml = function(file_path = "model.pmml") {
      export_to_pmml(self, file_path)
    },

    #' Train the multinomial logistic regression model
    fit = function(X, y, variable_selection = FALSE, use_parallel = FALSE) {
      fit_model(self, X, y, variable_selection, use_parallel)
    },

    #' Predict class labels
    predict = function(X) {
      predict_labels(self, X)
    },

    #' Predict class probabilities
    predict_proba = function(X) {
      predict_probabilities(self, X)
    },

    #' Perform variable selection
    var_select = function(X, y, threshold = 0.05, nbins = 5, B = 10000) {
      variable_selection(X, y, threshold, nbins, B)
    },

    #' Evaluate model performance
    evaluate = function(X, y) {
      evaluate_model(self, X, y)
    },

    #' Plot training loss
    plot_loss = function() {
      plot_training_loss(self)
    },

    #' Plot variable importance
    plot_importance = function() {
      plot_feature_importance(self)
    },

    #' Print a summary of the model
    print = function() {
      show_print(self)
    },

    #' Display detailed model summary
    summary = function() {
      show_summary(self)
    }
  )
)

