#' Perform Variable Selection
#'
#' Selects the most important variables based on a Chi-squared test.
#'
#' @param X A matrix or data frame of features.
#' @param y A vector of target labels (factor).
#' @param threshold Numeric. p-value threshold for variable selection. Default is 0.05.
#' @param nbins Integer. Number of bins for numeric variables. Default is 5.
#' @param B Integer. Number of Monte Carlo simulations for the Chi-squared test. Default is 10000.
#' @return A vector of selected feature names.
#' @export
#' @examples
#' \dontrun{
#' # Perform variable selection
#' selected_features <- model$var_select(X, y)
#' }
variable_selection <- function(X, y, threshold = 0.05, nbins = 5, B = 10000) {
  X_discrete <- X
  for (col_name in colnames(X_discrete)[-1]) {
    if (is.numeric(X_discrete[, col_name])) {
      X_discrete[, col_name] <- cut(X_discrete[, col_name], breaks = nbins, labels = FALSE)
    }
  }
  p_values <- sapply(colnames(X_discrete)[-1], function(col_name) {
    col <- X_discrete[, col_name]
    tbl <- table(col, y)
    if (all(dim(tbl) > 1)) {
      tryCatch({
        chisq <- chisq.test(tbl, simulate.p.value = TRUE, B = B)
        return(chisq$p.value)
      }, error = function(e) {
        warning(paste("Erreur pour la variable", col_name, ":", e$message))
        return(1)
      })
    } else {
      return(1)
    }
  })
  selected_features <- names(p_values[p_values < threshold])
  return(selected_features)
}
