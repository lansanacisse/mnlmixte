% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mnlmixte.R
\name{MNLMIXTE}
\alias{MNLMIXTE}
\title{Multinomial Logistic Regression Model for Mixed Data}
\description{
Multinomial Logistic Regression Model for Mixed Data

Multinomial Logistic Regression Model for Mixed Data
}
\details{
The \code{MNLMIXTE} R6 class implements multinomial logistic regression for mixed datasets
(categorical and numerical features). It supports variable selection, parallel processing,
and model evaluation. This class is designed to preprocess input data, fit a logistic regression
model, predict outcomes, and evaluate performance.
}
\examples{
\dontrun{
model <- MNLMIXTE$new(learning_rate = 0.01, epochs = 1000, regularization = 0.01)
data(students)
X <- students[, !(colnames(students) \%in\% "Target")]
y <- as.factor(students$Target)
model$fit(X, y, use_parallel = FALSE)
predictions <- model$predict(X)
probabilities <- model$predict_proba(X)
print(model$evaluate(X, y))
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{learning_rate}}{Numeric. Learning rate for gradient descent.}

\item{\code{epochs}}{Integer. Number of training epochs (iterations).}

\item{\code{coefficients}}{Matrix. Coefficients of the trained model.}

\item{\code{classes}}{Character vector. Unique class labels in the target variable.}

\item{\code{class_frequencies}}{Numeric vector. Proportions of each class in the training dataset.}

\item{\code{feature_names}}{Character vector. Names of features used during training.}

\item{\code{regularization}}{Numeric. Regularization strength to control overfitting.}

\item{\code{epsilon}}{Numeric. Small constant to avoid division by zero.}

\item{\code{loss_history}}{Numeric vector. Stores the evolution of the loss function during training.}

\item{\code{loss_function}}{Character. Specifies the loss function ("logloss" or "hinge").}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MNLMIXTE-new}{\code{MNLMIXTE$new()}}
\item \href{#method-MNLMIXTE-dependencies}{\code{MNLMIXTE$dependencies()}}
\item \href{#method-MNLMIXTE-preprocess_data}{\code{MNLMIXTE$preprocess_data()}}
\item \href{#method-MNLMIXTE-prepare_for_prediction}{\code{MNLMIXTE$prepare_for_prediction()}}
\item \href{#method-MNLMIXTE-var_importance}{\code{MNLMIXTE$var_importance()}}
\item \href{#method-MNLMIXTE-softmax}{\code{MNLMIXTE$softmax()}}
\item \href{#method-MNLMIXTE-to_pmml}{\code{MNLMIXTE$to_pmml()}}
\item \href{#method-MNLMIXTE-fit}{\code{MNLMIXTE$fit()}}
\item \href{#method-MNLMIXTE-predict}{\code{MNLMIXTE$predict()}}
\item \href{#method-MNLMIXTE-predict_proba}{\code{MNLMIXTE$predict_proba()}}
\item \href{#method-MNLMIXTE-var_select}{\code{MNLMIXTE$var_select()}}
\item \href{#method-MNLMIXTE-evaluate}{\code{MNLMIXTE$evaluate()}}
\item \href{#method-MNLMIXTE-plot_loss}{\code{MNLMIXTE$plot_loss()}}
\item \href{#method-MNLMIXTE-plot_importance}{\code{MNLMIXTE$plot_importance()}}
\item \href{#method-MNLMIXTE-print}{\code{MNLMIXTE$print()}}
\item \href{#method-MNLMIXTE-summary}{\code{MNLMIXTE$summary()}}
\item \href{#method-MNLMIXTE-clone}{\code{MNLMIXTE$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-new"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-new}{}}}
\subsection{Method \code{new()}}{
Initialize the multinomial logistic regression model with specified parameters.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$new(
  learning_rate = 0.01,
  epochs = 1000,
  regularization = 0.01,
  epsilon = 1e-08,
  loss_function = "logloss"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{learning_rate}}{Numeric. Learning rate for gradient descent. Default: 0.01.}

\item{\code{epochs}}{Integer. Number of training epochs. Default: 1000.}

\item{\code{regularization}}{Numeric. Regularization strength. Default: 0.01.}

\item{\code{epsilon}}{Numeric. Small constant to avoid division by zero. Default: 1e-8.}

\item{\code{loss_function}}{Character. Specifies the loss function ("logloss" or "hinge"). Default: "logloss".}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An initialized \code{MNLMIXTE} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-dependencies"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-dependencies}{}}}
\subsection{Method \code{dependencies()}}{
Check and install missing dependencies for the package.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$dependencies()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. Installs missing libraries if necessary.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-preprocess_data"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-preprocess_data}{}}}
\subsection{Method \code{preprocess_data()}}{
Preprocess data before training or prediction.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$preprocess_data(X)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{A matrix or data frame of features.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A preprocessed matrix.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-prepare_for_prediction"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-prepare_for_prediction}{}}}
\subsection{Method \code{prepare_for_prediction()}}{
Prepare data for prediction by ensuring alignment with trained model features.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$prepare_for_prediction(X)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{A matrix or data frame of features.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A matrix prepared for prediction.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-var_importance"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-var_importance}{}}}
\subsection{Method \code{var_importance()}}{
Compute the importance of each feature based on model coefficients.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$var_importance()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A named numeric vector of feature importances.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-softmax"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-softmax}{}}}
\subsection{Method \code{softmax()}}{
Apply the softmax function to a matrix of scores.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$softmax(z)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{z}}{A numeric matrix of scores.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A matrix of probabilities after applying the softmax transformation.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-to_pmml"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-to_pmml}{}}}
\subsection{Method \code{to_pmml()}}{
Export the trained model to PMML format.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$to_pmml(file_path = "model.pmml")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file_path}}{Character. File path for the PMML model.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None. Saves the PMML model to the specified location.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-fit"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-fit}{}}}
\subsection{Method \code{fit()}}{
Train the multinomial logistic regression model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$fit(X, y, variable_selection = FALSE, use_parallel = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{A matrix or data frame of features.}

\item{\code{y}}{A factor vector of target labels.}

\item{\code{variable_selection}}{Logical. Whether to perform variable selection. Default: FALSE.}

\item{\code{use_parallel}}{Logical. Whether to enable parallel computation. Default: FALSE.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None. Updates the model's coefficients and loss history.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-predict"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-predict}{}}}
\subsection{Method \code{predict()}}{
Predict class labels based on the trained model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$predict(X)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{A matrix or data frame of features.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A factor vector of predicted class labels.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-predict_proba"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-predict_proba}{}}}
\subsection{Method \code{predict_proba()}}{
Predict class probabilities for the input data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$predict_proba(X)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{A matrix or data frame of features.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A matrix of predicted probabilities for each class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-var_select"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-var_select}{}}}
\subsection{Method \code{var_select()}}{
Perform variable selection based on a Chi-squared test.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$var_select(X, y, threshold = 0.05, nbins = 5, B = 10000)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{A matrix or data frame of features.}

\item{\code{y}}{A factor vector of target labels.}

\item{\code{threshold}}{Numeric. P-value threshold for variable selection. Default: 0.05.}

\item{\code{nbins}}{Integer. Number of bins for discretizing numeric variables. Default: 5.}

\item{\code{B}}{Integer. Number of Monte Carlo simulations for the Chi-squared test. Default: 10000.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A vector of selected feature names.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-evaluate"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-evaluate}{}}}
\subsection{Method \code{evaluate()}}{
Evaluate model performance on test data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$evaluate(X, y)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{A matrix or data frame of features.}

\item{\code{y}}{A vector of true labels (factor).}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data frame of evaluation metrics for each class and overall averages.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-plot_loss"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-plot_loss}{}}}
\subsection{Method \code{plot_loss()}}{
Plot the loss function progression during training.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$plot_loss()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. Displays a plot of the loss evolution.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-plot_importance"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-plot_importance}{}}}
\subsection{Method \code{plot_importance()}}{
Visualize feature importance based on model coefficients.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$plot_importance()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. Displays a bar plot of feature importance.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-print"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-print}{}}}
\subsection{Method \code{print()}}{
Print a summary of the model, including the number of classes and features.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. Prints to the console.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-summary"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-summary}{}}}
\subsection{Method \code{summary()}}{
Display a detailed summary of the trained model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$summary()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None. Prints to the console.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MNLMIXTE-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MNLMIXTE-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MNLMIXTE$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
