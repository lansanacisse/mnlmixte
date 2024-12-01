# mnlmixte - multinomial logistic regression for mixed

## Overview

`mnlmixte` is an R package for performing multinomial logistic regression tailored for mixed datasets (categorical and numerical variables). The package is designed to handle both small and large-scale datasets, with support for parallel computation to optimize execution speed.

### Features

-   **Multinomial Logistic Regression:** Perform classification tasks on datasets with mixed variable types.
-   **Variable Selection:** Automatically identify important features using statistical tests.
-   **Parallel Processing:** Leverage multiple cores for efficient training on large datasets.
-   **Model Export:** Export trained models in PMML format for sharing and deployment.
-   **Visualization Tools:** Plot feature importance and loss curves.
-   **Evaluation Metrics:** Compute precision, recall, F1-score, and accuracy on test datasets.

## Installation

### Prerequisites

Make sure you have R (\>= 4.0.0) installed. The package relies on the following dependencies:

-   `FactoMineR`
-   `foreach`
-   `doParallel`
-   `parallel`
-   `pROC`
-   `caret`

### Installing from GitHub

To install the package directly from GitHub, run the following commands:

``` r
# Install devtools if you don't have it
install.packages(c("devtools", "usethis"))

# Install mnlmixte from GitHub
devtools::install_github("lansanacisse/mnlmixte")
```

### Installing Dependencies Manually

If needed, you can install the required dependencies separately:

``` r
install.packages(c("FactoMineR", "foreach", "doParallel", "parallel", "pROC", "caret"))
```

## Datasets

### Credit Score Classification Dataset

This dataset focuses on credit card clients' default payments, containing demographic and credit data from Taiwan. It includes 25 variables such as client ID, credit limit, gender, age, repayment status, and bill amounts. The primary goal is to analyze factors influencing default payments.

For more details, you can access the dataset documentation on Kaggle: [Multi-Class Classification Problem](https://www.kaggle.com/datasets/sudhanshu2198/processed-data-credit-score)

### Predict Students' Dropout and Academic Success Dataset

This dataset was created from a higher education institution to analyze factors influencing student dropout rates and academic success. It includes 36 features related to students' demographics, socio-economic status, and academic performance collected at enrollment and after the first two semesters. The primary goal is to build classification models that predict whether students will drop out, remain enrolled, or graduate.

For more details, you can access the dataset documentation here: [Predict Students' Dropout and Academic Success Dataset.](https://archive.ics.uci.edu/dataset/697/predict+students+dropout+and+academic+success)

## Usage

### Example Workflow

Here is a simple example of how to use the `mnlmixte` package:

``` r
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

# create a MNLMIXTE object
model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 5000, regularization = 0.01)

# Train the model
model$fit(X_train, y_train, variable_selection = TRUE, use_parallel=TRUE)

# Predict on the test data
predictions <- model$predict(X_test)

# Evaluate the model
results <- model$evaluate(X_test, y_test)
print(results, row.names = TRUE)
```

## Key Functions

### 1. `MNLMIXTE$new(...)`

-   Initializes the model with specified hyperparameters.
-   Parameters include:
    -   `learning_rate`: Learning rate for gradient descent.
    -   `epochs`: Number of training epochs.
    -   `regularization`: Regularization strength.
    -   `use_parallel`: Enable/disable parallel computation.

### 2. `fit(X, y, ...)`

-   Trains the model using the provided training data (`X` and `y`).
-   Supports variable selection and parallel computation.

### 3. `predict(X)`

-   Predicts class labels for a new dataset (`X`).

### 4. `predict_proba(X)`

-   Returns the probabilities for each class.

### 5. `plot_loss()`

-   Plots the loss curve over training epochs.

### 6. `plot_importance()`

-   Visualizes the importance of features.

### 7. `summary()`

-   Summarizes the trained model, including learning rate, epochs, and feature importance.

### 8. `evaluate(X, y)`

-   Computes precision, recall, F1-score, and accuracy on a test dataset.

### 9. `to_pmml(file_path)`

-   Exports the trained model to a PMML file for deployment.

## Contributing

We welcome contributions to the `mnlmixte` package! Please follow these steps:

```         
1. Fork the repository.
2. Create a feature branch: `git checkout -b feature-name`.
3. Commit your changes: `git commit -m "Add new feature"`.
4. Push to the branch: `git push origin feature-name`.
5. Submit a pull request.
```

## License

This package is licensed under the **GPL-3 License**. See the LICENSE file for details.

## Support

For any issues, please feel free to open an issue on the GitHub repository or contact the authors:

-   [Lansana CISSE](https://github.com/lansanacisse)
-   [Pierre Bourbon](https://github.com/pbrbn)
