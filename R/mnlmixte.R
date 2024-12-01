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
#' }
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
      self$learning_rate <- learning_rate
      self$epochs <- epochs
      self$regularization <- regularization
      self$epsilon <- epsilon
      self$coefficients <- NULL
      self$loss_history <- c()
      self$loss_function <- loss_function
    },

    #' @description
    #' Check and install missing dependencies for the package.
    #' @return None. Installs missing libraries if necessary.
    #' @export
    dependencies = function() {
      dependencies()
    },

    #' @description
    #' Preprocess data before training or prediction.
    #' @param X A matrix or data frame of features.
    #' @return A preprocessed matrix.
    #' @export
    preprocess_data = function(X) {
      preprocess_data(X)
    },

    #' @description
    #' Prepare data for prediction by ensuring alignment with trained model features.
    #' @param X A matrix or data frame of features.
    #' @return A matrix prepared for prediction.
    #' @export
    prepare_for_prediction = function(X) {
      prepare_for_prediction(X, self$feature_names)
    },

    #' @description
    #' Compute the importance of each feature based on model coefficients.
    #' @return A named numeric vector of feature importances.
    #' @export
    var_importance = function() {
      var_importance(self)
    },

    #' @description
    #' Apply the softmax function to a matrix of scores.
    #' @param z A numeric matrix of scores.
    #' @return A matrix of probabilities after applying the softmax transformation.
    #' @export
    softmax = function(z) {
      softmax(z)
    },

    #' @description
    #' Export the trained model to PMML format.
    #' @param file_path Character. File path for the PMML model.
    #' @return None. Saves the PMML model to the specified location.
    #' @export
    to_pmml = function(file_path = "model.pmml") {
      export_to_pmml(self, file_path)
    },

    #' @description
    #' Train the multinomial logistic regression model.
    #' @param X A matrix or data frame of features.
    #' @param y A factor vector of target labels.
    #' @param variable_selection Logical. Whether to perform variable selection. Default: FALSE.
    #' @param use_parallel Logical. Whether to enable parallel computation. Default: FALSE.
    #' @return None. Updates the model's coefficients and loss history.
    #' @export
    fit = function(X, y, variable_selection = FALSE, use_parallel = FALSE) {
      fit_model(self, X, y, variable_selection, use_parallel)
    },

    #' @description
    #' Predict class labels based on the trained model.
    #' @param X A matrix or data frame of features.
    #' @return A factor vector of predicted class labels.
    #' @export
    predict = function(X) {
      predict_labels(self, X)
    },

    #' @description
    #' Predict class probabilities for the input data.
    #' @param X A matrix or data frame of features.
    #' @return A matrix of predicted probabilities for each class.
    #' @export
    predict_proba = function(X) {
      predict_probabilities(self, X)
    },

    #' @description
    #' Perform variable selection based on a Chi-squared test.
    #' @param X A matrix or data frame of features.
    #' @param y A factor vector of target labels.
    #' @param threshold Numeric. P-value threshold for variable selection. Default: 0.05.
    #' @param nbins Integer. Number of bins for discretizing numeric variables. Default: 5.
    #' @param B Integer. Number of Monte Carlo simulations for the Chi-squared test. Default: 10000.
    #' @return A vector of selected feature names.
    #' @export
    var_select = function(X, y, threshold = 0.05, nbins = 5, B = 10000) {
      variable_selection(X, y, threshold, nbins, B)
    },

    #' @description
    #' Evaluate model performance on test data.
    #' @param X A matrix or data frame of features.
    #' @param y A vector of true labels (factor).
    #' @return A data frame of evaluation metrics for each class and overall averages.
    #' @export
    evaluate = function(X, y) {
      evaluate_model(self, X, y)
    },

    #' @description
    #' Plot the loss function progression during training.
    #' @return None. Displays a plot of the loss evolution.
    #' @export
    plot_loss = function() {
      plot_training_loss(self)
    },

    #' @description
    #' Visualize feature importance based on model coefficients.
    #' @return None. Displays a bar plot of feature importance.
    #' @export
    plot_importance = function() {
      plot_feature_importance(self)
    },

    #' @description
    #' Print a summary of the model, including the number of classes and features.
    #' @return None. Prints to the console.
    #' @export
    print = function() {
      show_print(self)
    },

    #' @description
    #' Display a detailed summary of the trained model.
    #' @return None. Prints to the console.
    #' @export
    summary = function() {
      show_summary(self)
    }
  )
)
