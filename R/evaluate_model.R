#' Evaluate Model Performance
#'
#' Evaluates the model on the given test data, computing metrics like precision, recall, F1-score, and accuracy.
#'
#' @param self The MNLMIXTE class object (required for accessing model parameters).
#' @param X A matrix or data frame of features.
#' @param y A vector of true labels (factor).
#' @return A data frame of evaluation metrics for each class and overall averages.
#' @export
#' @examples
#' \dontrun{
#' # Evaluate the model
#' results <- model$evaluate(X_test, y_test)
#' print(results)
#' }
evaluate_model <- function(self, X, y) {
  # Ensure the predict method in the class works correctly
  predictions <- self$predict(X)

  # Ensure y is a factor
  if (!is.factor(y)) {
    y <- as.factor(y)
    message("The target variable 'y' has been converted to a factor.")
  }

  # Ensure levels of y and predictions match
  if (!all(levels(y) %in% self$classes)) {
    stop("Levels in the target variable 'y' do not match the model's classes.")
  }

  # Confusion matrix
  confusion_matrix <- table(y, predictions)

  # Metrics for each class
  precision <- diag(confusion_matrix) / colSums(confusion_matrix)
  recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  support <- rowSums(confusion_matrix)

  # Handle divisions by zero (replace NaN with NA)
  precision[is.nan(precision)] <- NA
  recall[is.nan(recall)] <- NA
  f1_score[is.nan(f1_score)] <- NA

  # Global accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

  # Averages
  macro_avg <- c(mean(precision, na.rm = TRUE), mean(recall, na.rm = TRUE), mean(f1_score, na.rm = TRUE))
  weighted_avg <- c(
    sum(precision * support, na.rm = TRUE) / sum(support),
    sum(recall * support, na.rm = TRUE) / sum(support),
    sum(f1_score * support, na.rm = TRUE) / sum(support)
  )

  # Results data frame
  results <- data.frame(
    precision = c(precision, NA, macro_avg[1], weighted_avg[1]),
    recall = c(recall, NA, macro_avg[2], weighted_avg[2]),
    f1_score = c(f1_score, NA, macro_avg[3], weighted_avg[3]),
    support = c(support, sum(support), sum(support), sum(support))
  )

  # Row names for the results
  rownames(results) <- c(levels(y), "accuracy", "macro avg", "weighted avg")

  # Add accuracy to results
  results["accuracy", "precision"] <- accuracy
  results["accuracy", "recall"] <- accuracy
  results["accuracy", "f1_score"] <- accuracy

  return(results)
}
