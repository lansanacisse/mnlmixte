#' Predict Class Labels
#'
#' Predicts the class labels based on the input data and the model coefficients.
#'
#' @param self The `MNLMIXTE` R6 object.
#' @param X A matrix or data frame of features.
#' @return A factor vector of predicted class labels.
#' @export
predict_labels <- function(self, X) {
  if (is.null(self$coefficients)) {
    stop("The model has not been trained. Call fit() first.")
  }
  X <- prepare_for_prediction(X, self$feature_names)
  scores <- X %*% self$coefficients
  probs <- softmax(scores)
  predicted_class_indices <- apply(probs, 1, which.max)
  return(self$classes[predicted_class_indices])
}

#' Predict Class Probabilities
#'
#' Predicts the probabilities for each class based on the input data and the model coefficients.
#'
#' @param self The `MNLMIXTE` R6 object.
#' @param X A matrix or data frame of features.
#' @return A matrix of predicted probabilities, where each column corresponds to a class.
#' @export
predict_probabilities <- function(self, X) {
  if (is.null(self$coefficients)) {
    stop("The model has not been trained. Call fit() first.")
  }
  X <- prepare_for_prediction(X, self$feature_names)
  scores <- X %*% self$coefficients
  return(softmax(scores))
}
