# mnlmixte - Multinomial Logistic Regression for Mixed Data

## Overview

`mnlmixte` is an R package for performing multinomial logistic regression tailored for mixed datasets (categorical and numerical variables). The package is designed to handle both small and large-scale datasets, with support for parallel computation to optimize execution speed.

### Features

-   **Multinomial Logistic Regression:** Perform classification tasks on datasets with mixed variable types.
-   **Variable Selection:** Automatically identify important features using statistical tests (e.g., Chi-square).
-   **Parallel Processing:** Leverage multiple cores for efficient training on large datasets.
-   **Model Export:** Export trained models in PMML format for sharing and deployment.
-   **Visualization Tools:** Plot feature importance, loss curves, and ROC-AUC curves.
-   **Evaluation Metrics:** Compute precision, recall, F1-score, accuracy, and AUC on test datasets.

## Installation

### Prerequisites

Make sure you have R (\>= 4.0.0) installed. The package relies on the following dependencies:

-   `FactoMineR`
-   `foreach`
-   `doParallel`
-   `parallel`
-   `pROC`
-   `caret`
-   `ggplot2` (for visualization)

### Installing from GitHub

To install the package directly from GitHub, run the following commands:

```r
# Install devtools if you don't have it
install.packages("devtools")

# Install mnlmixte from GitHub
devtools::install_github("lansanacisse/mnlmixte")
```

### Installing Dependencies Manually

If needed, you can install the required dependencies separately:

```r
install.packages(c("FactoMineR", "foreach", "doParallel", "parallel", "pROC", "caret", "ggplot2","shiny", "shinydashboard", "shinyjs", "DT", "readxl", "ggplot2", "dplyr","reshape2 "))
```

## Shiny App Integration

The package includes a Shiny app for interactive exploration of multinomial logistic regression results. To launch the app:

```r
library(mnlmixte)
library(shiny)
runApp(system.file("shiny_app", package = "mnlmixte"), display.mode = "normal")
```

This app provides an intuitive user interface to preprocess data, train models, and visualize results.

## Datasets

### Included Dataset: Students

The `students` dataset included in the package is designed to analyze factors influencing student dropout rates and academic success. It includes 36 features related to students' demographics, socio-economic status, and academic performance. The goal is to predict whether students will drop out, remain enrolled, or graduate.

### Credit Score Classification Dataset (Optional)

The package can also be applied to the Credit Score Classification Dataset, which focuses on analyzing credit card clients' default payments. It includes 25 variables, such as demographic data, credit limit, repayment status, and bill amounts.

To access additional datasets, see:
- [Credit Score Dataset on Kaggle](https://www.kaggle.com/datasets/sudhanshu2198/processed-data-credit-score)
- [Students' Dropout and Academic Success Dataset](https://archive.ics.uci.edu/dataset/697/predict+students+dropout+and+academic+success)

## Usage

### Example Workflow with `students`

```r
library(caret)    # For data partitioning
library(mnlmixte) # mnlmixte package

# Load the students dataset
data(students)

# Prepare the data
set.seed(123) # For reproducibility
students$Target <- as.factor(students$Target)
X <- students[, !(colnames(students) %in% "Target")]
y <- students$Target

# Split into training and testing sets
train_idx <- sample(1:nrow(students), size = 0.7 * nrow(students))
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

# Create a MNLMIXTE object
model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 500, regularization = 0.01)

# Train the model
model$fit(X_train, y_train, variable_selection = TRUE, use_parallel = FALSE)

# Predict on the test data
predictions <- model$predict(X_test)

# Evaluate the model
results <- model$evaluate(X_test, y_test)
print(results)

# Plot results
model$plot_loss()
model$plot_importance()
```

## Key Functions

### Model Training and Prediction

1. **`MNLMIXTE$new(...)`**
   - Initializes the model with hyperparameters.
   - Parameters:
     - `learning_rate`: Learning rate for gradient descent.
     - `epochs`: Number of training iterations.
     - `regularization`: Regularization strength to reduce overfitting.
     - `loss_function`: Function for optimization (`logloss` or `hinge`).
     - `use_parallel`: Enables parallel processing.

2. **`fit(X, y, ...)`**
   - Trains the model using the provided features and target variable.

3. **`predict(X)`**
   - Predicts class labels for a given dataset.

4. **`predict_proba(X)`**
   - Predicts class probabilities for a given dataset.

### Model Evaluation and Visualization

1. **`evaluate(X, y)`**
   - Computes evaluation metrics: precision, recall, F1-score, accuracy, and AUC.

2. **`plot_loss()`**
   - Plots the evolution of loss during training.

3. **`plot_importance()`**
   - Displays feature importance in a bar plot.

4. **`plot_auc()`**
   - Visualizes the ROC curves and computes the AUC.

### Additional Features

1. **`var_select(X, y, ...)`**
   - Performs variable selection using Chi-square tests.

2. **`to_pmml(file_path)`**
   - Exports the trained model to a PMML file.

3. **`summary()`**
   - Summarizes model details, including hyperparameters and variable importance.



## Contributing

We welcome contributions to the `mnlmixte` package! Please follow these steps:

1. Fork the repository.
2. Create a feature branch: `git checkout -b feature-name`.
3. Commit your changes: `git commit -m "Add new feature"`.
4. Push to the branch: `git push origin feature-name`.
5. Submit a pull request.

## License

This package is licensed under the **GPL-3 License**. See the LICENSE file for details.

## Support

For any issues, please feel free to open an issue on the GitHub repository or contact the authors:

-   [Lansana CISSE](https://github.com/lansanacisse)
-   [Pierre Bourbon](https://github.com/pbrbn)
