#' Multinomial Logistic Regression Model for Mixed Data
#'
#' This R6 class implements multinomial logistic regression for mixed datasets
#' (categorical and numerical features). It supports variable selection, parallel processing,
#' and model evaluation.
#'
#' @field learning_rate Numeric. Learning rate for gradient descent.
#' @field epochs Integer. Number of epochs for training.
#' @field coefficients Matrix. Coefficients of the trained model.
#' @field classes Character. Class labels.
#' @field class_frequencies Numeric. Frequencies of each class in the training data.
#' @field feature_names Character. Names of features used in the model.
#' @field regularization Numeric. Regularization strength.
#' @field epsilon Numeric. Small constant to avoid division by zero.
#' @field loss_history Numeric. History of loss values during training.
#' @field loss_function Character. Loss function used ("logloss" or "hinge").
#' @field use_parallel Logical. Whether parallel computation is enabled.
#'
MNLMIXTE <- R6::R6Class(
  "MNLMIXTE",

  public = list(
    # Attributs publics
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
    use_parallel = NULL,

#' Check and Install Missing Dependencies
#'
#' This method checks if required libraries are installed. If not, it installs them.
#'
#' @return None. Installs missing libraries if necessary.

    dependencies = function() {
      required_libraries <- c("FactoMineR", "pROC", "parallel", "foreach", "doParallel", "caret")
      missing_libraries <- required_libraries[!sapply(required_libraries, requireNamespace, quietly = TRUE)]

      if (length(missing_libraries) > 0) {
        message("The following libraries are missing: ", paste(missing_libraries, collapse = ", "))
        message("Installing missing libraries...")
        install.packages(missing_libraries)
        missing_after_install <- required_libraries[!sapply(required_libraries, requireNamespace, quietly = TRUE)]
        if (length(missing_after_install) > 0) {
          stop("Failed to install the following libraries: ", paste(missing_after_install, collapse = ", "))
        }
        message("All required libraries have been successfully installed.")
      } else {
        # Suppress the redundant message
        # message("All required libraries are already installed.")
      }
    },
#' Initialize the Multinomial Logistic Regression Model
#'
#' Sets up the model with the provided hyperparameters and checks for dependencies.
#'
#' @param learning_rate Numeric. Learning rate for gradient descent. Default is 0.01.
#' @param epochs Integer. Number of epochs for training. Default is 1000.
#' @param regularization Numeric. Regularization strength. Default is 0.01.
#' @param epsilon Numeric. Small constant to avoid division by zero in calculations. Default is 1e-8.
#' @param loss_function Character. Loss function to use. Options are "logloss" or "hinge". Default is "logloss".
#' @param use_parallel Logical. Whether to enable parallel computation. Default is FALSE.
#' @return An initialized `MNLMIXTE` object.
#'
#' @export
#' @examples
#' library(mnlmixte)
#' Initialize the model
#' model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 1000, use_parallel = FALSE)
    initialize = function(learning_rate = 0.01, epochs = 1000, regularization = 0.01, epsilon = 1e-8, loss_function = "logloss") {
      self$dependencies()  # Vérifiez et installez les dépendances si nécessaire
      self$learning_rate <- learning_rate
      self$epochs <- epochs
      self$regularization <- regularization
      self$epsilon <- epsilon
      self$coefficients <- NULL
      self$loss_history <- c()
      self$loss_function <- loss_function
    },

#' Train the Model
#'
#' Trains the multinational logistic regression model on the given dataset.
#'
#' @param X A matrix or data frame of features.
#' @param y A factor vector of target labels.
#' @param variable_selection Logical. Whether to perform variable selection. Default is FALSE.
#' @param use_parallel Logical. Whether to use parallel computation. Default is FALSE.
#' @return None. Updates the model's coefficients and loss history.
#' @export
#' @examples
#' Train the model
#' model$fit(X_train, y_train)
    fit = function(X, y, variable_selection = FALSE, use_parallel = FALSE) {
      if (nrow(X) != length(y)) {
        stop("X et y doivent avoir le même nombre de lignes.")
      }
      if (any(is.na(X))) {
        stop("X contient des valeurs manquantes.")
      }
      if (any(is.na(y))) {
        stop("y contient des valeurs manquantes.")
      }

      if (!is.factor(y)) {
        y <- as.factor(y)
        message("La variable cible 'y' a été convertie en facteur.")
      }
      self$classes <- levels(y)
      self$class_frequencies <- table(y) / length(y)

      # Prétraitement des données
      X <- private$preprocess_data(X)
      X <- cbind(Intercept = 1, X)
      self$feature_names <- colnames(X)

      n <- nrow(X)
      k <- length(self$classes)

      if (n == 0 || k == 0) stop("Les données sont invalides après prétraitement.")

      # Sélection automatique des variables
      if (variable_selection) {
        selected_features <- self$var_select(X, y)
        if (length(selected_features) == 0) stop("Aucune variable sélectionnée.")
        X <- X[, c("Intercept", selected_features), drop = FALSE]
        self$feature_names <- colnames(X)
      }

      # Initialisation des coefficients
      self$coefficients <- matrix(rnorm(ncol(X) * k, mean = 0, sd = 0.01), nrow = ncol(X), ncol = k)

      # Encodage de y en one-hot
      y_matrix <- model.matrix(~ y - 1)

      # Gestion parallèle ou séquentielle
      if (use_parallel) {
        message("Utilisation du mode parallèle...")
        cl <- makeCluster(detectCores() - 1)  # Création d'un cluster de calcul
        clusterExport(cl, varlist = c("X", "y_matrix", "self"), envir = environment())
        clusterEvalQ(cl, library(parallel))

        loss_gradient <- parLapply(cl, 1:self$epochs, function(epoch) {
          scores <- X %*% self$coefficients
          probs <- private$softmax(scores)
          error <- probs - y_matrix
          gradient <- t(X) %*% error / n + self$regularization * self$coefficients

          # Calcul de la perte
          loss <- -sum(y_matrix * log(probs + self$epsilon)) / n
          self$coefficients <- self$coefficients - self$learning_rate * gradient

          if (epoch %% 100 == 0) {
            message("Époque : ", epoch, " - Perte : ", loss)
          }

          return(loss)
        })
        stopCluster(cl)  # Arrêter le cluster
        self$loss_history <- unlist(loss_gradient)
      } else {
        message("Utilisation du mode séquentiel...")
        for (epoch in 1:self$epochs) {
          scores <- X %*% self$coefficients
          probs <- private$softmax(scores)
          error <- probs - y_matrix
          gradient <- t(X) %*% error / n + self$regularization * self$coefficients
          self$coefficients <- self$coefficients - self$learning_rate * gradient

          # Calcul de la perte
          loss <- -sum(y_matrix * log(probs + self$epsilon)) / n
          self$loss_history <- c(self$loss_history, loss)

          if (epoch %% 100 == 0) {
            message("Époque : ", epoch, " - Perte : ", loss)
          }
        }
      }
    },

#' Predict Class Labels
#'
#' Predicts the class labels for the given input data.
#'
#' @param X A matrix or data frame of features.
#' @return A factor vector of predicted class labels.
#' @export
#' @examples
#' predictions <- model$predict(X_test)

    predict = function(X) {
      X <- private$prepare_for_prediction(X)
      probs <- private$softmax(X %*% self$coefficients)
      predicted_class_indices <- apply(probs, 1, which.max)
      return(self$classes[predicted_class_indices])
    },

#' Predict Class Probabilities
#'
#' Predicts the probabilities for each class for the given input data.
#'
#' @param X A matrix or data frame of features.
#' @return A matrix of predicted probabilities, where each column corresponds to a class.
#' @export
#' @examples
#' probabilities <- model$predict_proba(X_test)
    predict_proba = function(X) {
      X <- private$prepare_for_prediction(X)
      return(private$softmax(X %*% self$coefficients))
    },

#' Plot Training Loss
#'
#' Visualizes the evolution of the loss function during training.
#'
#' @return None. Displays a plot of the loss function.
#' @export
#' @examples
#' model$plot_loss()
    plot_loss = function() {
      if (length(self$loss_history) == 0) stop("Le modèle n'a pas encore été entraîné.")
      plot(self$loss_history, type = "l", col = "blue", lwd = 2,
           xlab = "Époques", ylab = "Perte", main = "Évolution de la Perte")
    },

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
#' selected_features <- model$var_select(X, y)
    var_select = function(X, y, threshold = 0.05, nbins = 5, B = 10000) {
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
    },

#' Compute Variable Importance
#'
#' Calculates the importance of each feature based on the magnitude of model coefficients.
#'
#' @return A named numeric vector of feature importances.
#' @export
#' @examples
#' importance <- model$var_importance()
#' print(importance)

    var_importance = function() {
      if (is.null(self$coefficients)) stop("Les coefficients ne sont pas initialisés.")
      importance <- rowSums(abs(self$coefficients))
      names(importance) <- self$feature_names
      return(sort(importance[-1], decreasing = TRUE))  # Exclure l'intercept
    },

#' Plot Variable Importance
#'
#' Visualizes the importance of variables using a bar plot.
#'
#' @return None. Displays a bar plot.
#' @export
#' @examples
#' model$plot_importance()
    plot_importance = function() {
      importance <- self$var_importance()
      barplot(importance, main = "Importance des Variables", col = "blue", las = 2)
    },

#' Export Model to PMML
#'
#' Saves the trained model in PMML (Predictive Model Markup Language) format.
#'
#' @param file_path Character. File path where the PMML model will be saved. Default is "model.pmml".
#' @return None. Saves the PMML model to the specified file path.
#' @export
#' @examples
#' model$to_pmml("model.pmml")

    to_pmml = function(file_path = "model.pmml") {
      if (is.null(self$coefficients)) {
        stop("Erreur : Les coefficients ne sont pas disponibles. Entraînez d'abord le modèle.")
      }
      pmml_content <- paste0(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<PMML version=\"4.4\" xmlns=\"http://www.dmg.org/PMML-4_4\">\n",
        "  <Header>\n",
        "    <Application name=\"MNLM\" version=\"1.0\"/>\n",
        "    <Timestamp>", Sys.time(), "</Timestamp>\n",
        "  </Header>\n",
        "  <DataDictionary>\n",
        paste0(
          "    <DataField name=\"", self$feature_names, "\" optype=\"continuous\" dataType=\"double\"/>\n",
          collapse = ""
        ),
        "    <DataField name=\"target\" optype=\"categorical\" dataType=\"string\"/>\n",
        "  </DataDictionary>\n",
        "  <RegressionModel modelName=\"MNLM\" functionName=\"classification\">\n",
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
      cat("Modèle exporté en PMML et sauvegardé dans :", file_path, "\n")
    },

#' Print Model Summary
#'
#' Prints a summary of the model, including the number of classes and features.
#'
#' @return None. Prints to the console.
#' @export
#' @examples
#' model$print()

    print = function() {
      cat("Modèle de Régression Logistique Multinomiale\n")
      cat("Classes :", paste(self$classes, collapse = ", "), "\n")
      cat("Nombre de caractéristiques :", length(self$feature_names) - 1, "\n")
    },

#' Display Model Summary
#'
#' Displays a detailed summary of the trained model, including learning rate, epochs, class frequencies, and feature importance.
#'
#' @return None. Prints to the console.
#' @export
#' @examples
#' model$summary()

    summary = function() {
      if (is.null(self$coefficients)) {
        cat("Le modèle n'a pas encore été entraîné.\n")
        return()
      }
      cat("Résumé du Modèle\n")
      cat("Taux d'Apprentissage :", self$learning_rate, "\n")
      cat("Époques :", self$epochs, "\n")
      cat("Classes :", paste(self$classes, collapse = ", "), "\n")
      cat("Fréquence des Classes :\n")
      print(self$class_frequencies)
      cat("Importance des Variables :\n")
      print(self$var_importance())
    },
#' Evaluate Model Performance
#'
#' Evaluates the model on the given test data, computing metrics like precision, recall, F1-score, and accuracy.
#'
#' @param X A matrix or data frame of features.
#' @param y A vector of true labels (factor).
#' @return A data frame of evaluation metrics for each class and overall averages.
#' @export
#' @examples
#' results <- model$evaluate(X_test, y_test)
#' print(results)

    evaluate = function(X, y) {
      predictions <- self$predict(X)
      if (!is.factor(y)) {
        y <- as.factor(y)
      }

      # Calculer la matrice de confusion
      confusion_matrix <- table(y, predictions)

      # Calculer les métriques pour chaque classe
      classes <- levels(y)
      precision <- diag(confusion_matrix) / colSums(confusion_matrix)
      recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
      f1_score <- 2 * (precision * recall) / (precision + recall)
      support <- rowSums(confusion_matrix)

      # Calculer l'exactitude globale
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

      # Calculer les moyennes
      macro_avg <- c(mean(precision), mean(recall), mean(f1_score), sum(support))
      weighted_avg <- c(
        sum(precision * support) / sum(support),
        sum(recall * support) / sum(support),
        sum(f1_score * support) / sum(support),
        sum(support)
      )

      # Créer un data frame avec les résultats
      results <- data.frame(
        precision = c(precision, NA, macro_avg[1], weighted_avg[1]),
        recall = c(recall, NA, macro_avg[2], weighted_avg[2]),
        f1_score = c(f1_score, NA, macro_avg[3], weighted_avg[3]),
        support = c(support, sum(support), sum(support), sum(support))
      )

      rownames(results) <- c(classes, "accuracy", "macro avg", "weighted avg")

      # Formater les résultats
      results_formatted <- format(results, digits = 2, nsmall = 2)
      results_formatted$support <- format(results$support, nsmall = 0)

      # Ajouter l'exactitude globale
      results_formatted["accuracy", "f1_score"] <- sprintf("%.2f", accuracy)
      results_formatted["accuracy", c("precision", "recall")] <- ""

      return(results_formatted)
    }

  ),
  private = list(
    preprocess_data = function(X) {
      numeric_cols <- sapply(X, is.numeric)
      if (any(numeric_cols)) {
        X[, numeric_cols] <- scale(X[, numeric_cols])
      }
      X_processed <- model.matrix(~ . - 1, data = X)
      return(as.matrix(X_processed))
    },
    prepare_for_prediction = function(X) {
      X <- private$preprocess_data(X)
      X <- cbind(Intercept = 1, X)
      missing_cols <- setdiff(self$feature_names, colnames(X))
      for (col in missing_cols) {
        X <- cbind(X, 0)
        colnames(X)[ncol(X)] <- col
      }
      return(X[, self$feature_names, drop = FALSE])
    },
    softmax = function(z) {
      z_max <- apply(z, 1, max)
      exp_z <- exp(z - z_max)
      exp_z / rowSums(exp_z)
    }
  )
)
