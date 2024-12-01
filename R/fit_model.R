#' Train the Multinomial Logistic Regression Model
#'
#' This function trains the multinomial logistic regression model on the provided dataset.
#'
#' @param self An instance of the MNLMIXTE class.
#' @param X A matrix or data frame of features.
#' @param y A factor vector of target labels.
#' @param variable_selection Logical. If `TRUE`, performs variable selection. Default: FALSE.
#' @param use_parallel Logical. If `TRUE`, enables parallel computation. Default: FALSE.
#' @return None. Updates the model's coefficients and loss history in the MNLMIXTE instance.
#' @export
fit_model <- function(self, X, y, variable_selection = FALSE, use_parallel = FALSE) {
  # Ensure matching number of rows in X and y
  if (nrow(X) != length(y)) {
    stop("X and y must have the same number of rows.")
  }

  # Check for missing values
  if (any(is.na(X))) {
    stop("X contains missing values.")
  }
  if (any(is.na(y))) {
    stop("y contains missing values.")
  }

  # Convert y to factor if needed
  if (!is.factor(y)) {
    y <- as.factor(y)
    message("The target variable 'y' has been converted to a factor.")
  }

  # Assign class-related properties
  self$classes <- levels(y)
  self$class_frequencies <- table(y) / length(y)

  # Use preprocess_data from utils.R
  X <- preprocess_data(X)  # Scale numerical data and process the matrix
  X <- cbind(Intercept = 1, X)  # Add intercept column
  self$feature_names <- colnames(X)

  n <- nrow(X)
  k <- length(self$classes)

  if (n == 0 || k == 0) stop("The data is invalid after preprocessing.")

  # Variable selection (optional)
  if (variable_selection) {
    selected_features <- self$var_select(X, y)
    if (length(selected_features) == 0) stop("No variables were selected.")
    X <- X[, c("Intercept", selected_features), drop = FALSE]
    self$feature_names <- colnames(X)
  }

  # Initialize coefficients
  self$coefficients <- matrix(rnorm(ncol(X) * k, mean = 0, sd = 0.01), nrow = ncol(X), ncol = k)

  # One-hot encode y
  y_matrix <- model.matrix(~ y - 1)

  # Sequential or parallel processing
  if (use_parallel) {
    message("Using parallel mode...")
    cl <- makeCluster(detectCores() - 1)  # Create a cluster
    clusterExport(cl, varlist = c("X", "y_matrix", "self"), envir = environment())
    clusterEvalQ(cl, library(parallel))

    loss_gradient <- parLapply(cl, 1:self$epochs, function(epoch) {
      scores <- X %*% self$coefficients
      probs <- softmax(scores)  # Use softmax from utils.R
      error <- probs - y_matrix
      gradient <- t(X) %*% error / n + self$regularization * self$coefficients

      # Calculate loss
      loss <- -sum(y_matrix * log(probs + self$epsilon)) / n
      self$coefficients <- self$coefficients - self$learning_rate * gradient

      if (epoch %% 100 == 0) {
        message("Epoch: ", epoch, " - Loss: ", loss)
      }

      return(loss)
    })
    stopCluster(cl)
    self$loss_history <- unlist(loss_gradient)
  } else {
    message("Using sequential mode...")
    for (epoch in 1:self$epochs) {
      scores <- X %*% self$coefficients
      probs <- softmax(scores)  # Use softmax from utils.R
      error <- probs - y_matrix
      gradient <- t(X) %*% error / n + self$regularization * self$coefficients
      self$coefficients <- self$coefficients - self$learning_rate * gradient

      # Calculate loss
      loss <- -sum(y_matrix * log(probs + self$epsilon)) / n
      self$loss_history <- c(self$loss_history, loss)

      if (epoch %% 100 == 0) {
        message("Epoch: ", epoch, " - Loss: ", loss)
      }
    }
  }
}

