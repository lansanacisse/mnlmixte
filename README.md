# mnlmixte - multinomial logistic regression  for mixed
  
**Author(s):**  
- Lansana CISSE (<l.cisse@univ-lyon2.fr>)  
- Pierre Bourdon (<p.bourdon@univ-lyon2.fr>)  

---

## Overview

`mnlmixte` is an R package for performing multinomial logistic regression tailored for mixed datasets (categorical and numerical variables). The package is designed to handle both small and large-scale datasets, with support for parallel computation to optimize execution speed.

### Features

- **Multinomial Logistic Regression:** Perform classification tasks on datasets with mixed variable types.
- **Variable Selection:** Automatically identify important features using statistical tests.
- **Parallel Processing:** Leverage multiple cores for efficient training on large datasets.
- **Model Export:** Export trained models in PMML format for sharing and deployment.
- **Visualization Tools:** Plot feature importance and loss curves.
- **Evaluation Metrics:** Compute precision, recall, F1-score, and accuracy on test datasets.

---

## Installation

### Prerequisites
Make sure you have R (>= 4.0.0) installed. The package relies on the following dependencies:

- `FactoMineR`
- `foreach`
- `doParallel`
- `parallel`
- `pROC`
- `caret`

### Installing from GitHub

To install the package directly from GitHub, run the following commands:

```R
# Install devtools if you don't have it
install.packages("devtools")

# Install mnlmixte from GitHub
devtools::install_github("lansanacisse/mnlmixte")
```

### Installing Dependencies Manually
If needed, you can install the required dependencies separately:

```R
install.packages(c("FactoMineR", "foreach", "doParallel", "parallel", "pROC", "caret"))
```

---

## Usage

### Example Workflow

Here is a simple example of how to use the `mnlmixte` package:

```R
# Load the mnlmixte library
library(mnlmixte)

# Create some training data (replace with your own dataset)
X_train <- data.frame(
  Var1 = sample(1:100, 100, replace = TRUE),
  Var2 = sample(c("A", "B", "C"), 100, replace = TRUE)
)
y_train <- as.factor(sample(c("Yes", "No"), 100, replace = TRUE))

# Initialize the model
model <- MNLMIXTE$new(
  learning_rate = 0.01,
  epochs = 500,
  regularization = 0.01,
  use_parallel = TRUE
)

# Train the model
model$fit(X_train, y_train)

# Summarize the model
model$summary()

# Predict on new data
predictions <- model$predict(X_train)
print(predictions)

# Plot feature importance
model$plot_importance()
```

---

## Key Functions

### 1. `MNLMIXTE$new(...)`
- Initializes the model with specified hyperparameters.
- Parameters include:
  - `learning_rate`: Learning rate for gradient descent.
  - `epochs`: Number of training epochs.
  - `regularization`: Regularization strength.
  - `use_parallel`: Enable/disable parallel computation.

### 2. `fit(X, y, ...)`
- Trains the model using the provided training data (`X` and `y`).
- Supports variable selection and parallel computation.

### 3. `predict(X)`
- Predicts class labels for a new dataset (`X`).

### 4. `predict_proba(X)`
- Returns the probabilities for each class.

### 5. `plot_loss()`
- Plots the loss curve over training epochs.

### 6. `plot_importance()`
- Visualizes the importance of features.

### 7. `summary()`
- Summarizes the trained model, including learning rate, epochs, and feature importance.

### 8. `evaluate(X, y)`
- Computes precision, recall, F1-score, and accuracy on a test dataset.

### 9. `to_pmml(file_path)`
- Exports the trained model to a PMML file for deployment.

---

## Contributing

We welcome contributions to the `mnlmixte` package! Please follow these steps:

1. Fork the repository.
2. Create a feature branch: `git checkout -b feature-name`.
3. Commit your changes: `git commit -m "Add new feature"`.
4. Push to the branch: `git push origin feature-name`.
5. Submit a pull request.

---

## License

This package is licensed under the **GPL-3 License**. See the LICENSE file for details.

---

## Support

For any issues, please feel free to open an issue on the GitHub repository or contact the authors:

- Lansana CISSE (<l.cisse@univ-lyon2.fr>)
- Pierre Bourdon (<p.bourdon@univ-lyon2.fr>)

---

### Repository Link:
[GitHub - lansanacisse/mnlmixte](https://github.com/lansanacisse/mnlmixte)

---
