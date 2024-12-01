#' @importFrom utils install.packages
#' @importFrom stats rnorm model.matrix chisq.test
#' @importFrom graphics barplot
#' @importFrom graphics plot
#' Check and Install Missing Dependencies
#'
#' This method checks if the required libraries are installed. If any are missing, it attempts to install them.
#'
#' @return None. Installs missing libraries if necessary.
#' @export
dependencies <- function() {
  required_libraries <- c("FactoMineR", "pROC", "parallel", "foreach", "doParallel", "caret")
  missing_libraries <- required_libraries[!sapply(required_libraries, requireNamespace, quietly = TRUE)]

  if (length(missing_libraries) > 0) {
    message("Installing missing libraries: ", paste(missing_libraries, collapse = ", "))
    install.packages(missing_libraries)
  } else {
    message("All required libraries are already installed.")
  }
}

#' Print Model Summary
#' @return None. Prints to the console.
#' @export
show_print <- function(self) {
  cat("Multinomial Logistic Regression Model\n")
  cat("Classes:", paste(self$classes, collapse = ", "), "\n")
  cat("Number of features:", length(self$feature_names) - 1, "\n")
}

#' Display Model Summary
#' @return None. Prints to the console.
#' @export
show_summary <- function(self) {
  if (is.null(self$coefficients)) {
    cat("The model has not been trained yet.\n")
    return()
  }
  cat("Model Summary\n")
  cat("Learning Rate:", self$learning_rate, "\n")
  cat("Epochs:", self$epochs, "\n")
  cat("Classes:", paste(self$classes, collapse = ", "), "\n")
  cat("Class Frequencies:\n")
  print(self$class_frequencies)
  cat("Feature Importance:\n")
  print(self$var_importance())
}

#' Compute Variable Importance
#' @return A named numeric vector of feature importances.
#' @export
var_importance <- function(self) {
  if (is.null(self$coefficients)) stop("Coefficients are not initialized.")
  importance <- rowSums(abs(self$coefficients))
  names(importance) <- self$feature_names
  return(sort(importance[-1], decreasing = TRUE))  # Exclude the intercept
}

#' Preprocess Data
#' @param X A data frame or matrix of features.
#' @return A preprocessed matrix.
#' @export
preprocess_data <- function(X) {
  numeric_cols <- sapply(X, is.numeric)
  if (any(numeric_cols)) {
    X[, numeric_cols] <- scale(X[, numeric_cols])
  }
  X_processed <- model.matrix(~ . - 1, data = X)
  return(as.matrix(X_processed))
}

#' Prepare Data for Prediction
#' @param X A matrix or data frame of features.
#' @param feature_names Character vector of feature names (from the trained model).
#' @return A matrix prepared for prediction.
#' @export
prepare_for_prediction <- function(X, feature_names) {
  X <- preprocess_data(X)
  X <- cbind(Intercept = 1, X)
  missing_cols <- setdiff(feature_names, colnames(X))
  for (col in missing_cols) {
    X <- cbind(X, 0)
    colnames(X)[ncol(X)] <- col
  }
  return(X[, feature_names, drop = FALSE])
}

#' Softmax Function
#'
#' Applies the softmax function to a matrix of scores.
#'
#' @param z A numeric matrix of scores.
#' @return A matrix of probabilities after applying the softmax transformation.
#' @export
softmax <- function(z) {
  z <- as.matrix(z)
  z_max <- apply(z, 1, max) # Avoid overflow by subtracting max
  exp_z <- exp(z - z_max)
  return(exp_z / rowSums(exp_z))
}


#' Export Model to PMML
#' @param self The MNLMIXTE object.
#' @param file_path Character. File path for the PMML model.
#' @return None. Saves the PMML model.
#' @export
export_to_pmml <- function(self, file_path = "model.pmml") {
  if (is.null(self$coefficients)) stop("Train the model before exporting to PMML.")
  pmml_content <- paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "<PMML version=\"4.4\" xmlns=\"http://www.dmg.org/PMML-4_4\">\n",
    "  <Header>\n",
    "    <Application name=\"MNLMIXTE\" version=\"1.0\"/>\n",
    "    <Timestamp>", Sys.time(), "</Timestamp>\n",
    "  </Header>\n",
    "  <DataDictionary>\n",
    paste0(
      "    <DataField name=\"", self$feature_names, "\" optype=\"continuous\" dataType=\"double\"/>\n",
      collapse = ""
    ),
    "    <DataField name=\"target\" optype=\"categorical\" dataType=\"string\"/>\n",
    "  </DataDictionary>\n",
    "  <RegressionModel modelName=\"MNLMIXTE\" functionName=\"classification\">\n",
    paste0(
      "    <RegressionTable targetCategory=\"", self$classes, "\">\n",
      paste0(
        "      <NumericPredictor name=\"", self$feature_names, "\" coefficient=\"", self$coefficients[, 1], "\"/>\n",
        collapse = ""
      ),
      "    </RegressionTable>\n",
      collapse = ""
    ),
    "  </RegressionModel>\n",
    "</PMML>\n"
  )
  write(pmml_content, file = file_path)
  cat("Model exported to PMML at:", file_path, "\n")
}
