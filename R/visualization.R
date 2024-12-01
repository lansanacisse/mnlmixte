#' Plot Training Loss
#'
#' Visualizes the evolution of the loss function during training.
#'
#' @return None. Displays a plot of the loss function.
#' @export
#' @examples
#' \dontrun{
#' # Plot the loss history
#' model$plot_loss()
#' }
plot_training_loss <- function(self) {
  if (length(self$loss_history) == 0) stop("The model has not been trained yet.")
  plot(self$loss_history, type = "l", col = "blue", lwd = 2,
       xlab = "Epochs", ylab = "Loss", main = "Loss Evolution")
}

#' Plot Variable Importance
#'
#' Visualizes the importance of variables using a bar plot.
#'
#' @return None. Displays a bar plot.
#' @export
#' @examples
#' \dontrun{
#' #' # Plot feature importance
#' model$plot_importance()
#' }

plot_feature_importance <- function(self) {
  importance <- self$var_importance()
  barplot(importance, main = "Feature Importance", col = "blue", las = 2)
}

