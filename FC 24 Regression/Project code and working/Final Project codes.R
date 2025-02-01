library(caret)     
library(glmnet) 
library(readxl) 
library(ggplot2)
library(car)
library(gridExtra)
library(scales)
library(corrplot)

data <- read_excel("Dataset.xlsx")

# Preprocess the data
# 1. Remove the last 35 columns
data <- data[, -((ncol(data) - 34):ncol(data))]
colnames(data)

# 2. Remove rows with any missing values
data <- na.omit(data)
sum(is.na(data))  # Check for any remaining NA values

# 3. Select relevant columns for analysis
selected_columns <- c('overall', 'wage_eur', 'age', 'value_eur', 'pace', 
                      'shooting', 'passing', 'dribbling', 'defending', 
                      'physic', 'skill_moves', 'weak_foot')
data <- data[, selected_columns]

# Split the data into predictors (X) and target variable (y)
X <- data[, -1]    # Remove 'overall' from predictors
y <- data$overall

####################
### EDA ####
#############

ggplot(data, aes(x = overall)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Overall Ratings", x = "Overall Rating", y = "Density") +
  theme_minimal()

# Scatter plot: Wage vs Overall Rating with formatted x-axis
ggplot(data, aes(x = wage_eur, y = overall)) +
  geom_point(color = "orange", alpha = 0.5) +
  scale_x_continuous(labels = comma) +
  labs(title = "Wage vs Overall Rating", x = "Wage", y = "Overall Rating")

# Boxplot: Overall by Age Groups (Binned)
# Create age groups, including an "above 40" category
df <- data %>% mutate(age_group = cut(age, breaks = c(15, 20, 25, 30, 35, 40), labels = c("15-20", "21-25", "26-30", "31-35", "36-40")))

# Plot Overall Rating by Age Group, including age 40+
ggplot(df, aes(x = age_group, y = overall)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "Overall Rating by Age Group", x = "Age Group", y = "Overall Rating")

#### age  40 and above are outliers

### Correlattion
correlation_matrix <- cor(selected_data, use = "complete.obs")

# Create the correlation plot with values displayed
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # Add correlation values in black
         number.cex = 0.7, # Adjust size of correlation numbers
         title = "Correlation Map of Key Variables", mar = c(0, 0, 1, 0))

########################
######normal ####
####################


### Multiple Linear Regression on the entire dataset
linear_model <- lm(overall ~ ., data = data)
linear_model_summary <- summary(linear_model)
y_pred_linear <- predict(linear_model, newdata = data)

# Calculate MSE for Multiple Linear Regression
mse_linear <- mean((y - y_pred_linear)^2)

# Print Multiple Linear Regression summary and MSE
cat("Multiple Linear Regression Model Summary:\n")
print(linear_model_summary)
cat("\nMultiple Linear Regression MSE:", mse_linear, "\n")

### Polynomial Regression (degree = 2) with selective linear terms
# Separate the predictors for linear-only and polynomial terms
linear_terms <- data[, c('wage_eur', 'age', 'value_eur')]
poly_terms <- data[, c('pace', 'shooting', 'passing', 'dribbling', 
                       'defending', 'physic', 'skill_moves', 'weak_foot')]

# Generate both linear and squared polynomial features for selected predictors
poly_features <- as.data.frame(poly(as.matrix(poly_terms), degree = 2, raw = TRUE))

# Combine linear terms, original polynomial terms, squared features, and target variable
data_poly <- cbind(data$overall, linear_terms, poly_features)
colnames(data_poly)[1] <- "overall"  # Rename target variable column

# Fit the polynomial regression model
poly_model <- lm(overall ~ ., data = data_poly)
poly_model_summary <- summary(poly_model)
y_pred_poly <- predict(poly_model, newdata = data_poly)

# Calculate MSE for Polynomial Regression
mse_poly <- mean((data$overall - y_pred_poly)^2)

# Print Polynomial Regression summary and MSE
cat("\nPolynomial Regression Model Summary:\n")
print(poly_model_summary)
cat("\nPolynomial Regression with Linear Terms (wage, age, value) - MSE:", mse_poly, "\n")

##########################
### external validation###
#########################

# Split the data into training (60%), validation (20%), and test (20%) sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.6, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]

# Remaining data (40%) for validation and testing
remaining_data <- data[-trainIndex, ]
remaining_X <- X[-trainIndex, ]
remaining_y <- y[-trainIndex]

# Split the remaining data into validation and test sets (each 20% of the original data)
validationIndex <- createDataPartition(remaining_y, p = 0.5, list = FALSE)
X_val <- remaining_X[validationIndex, ]
y_val <- remaining_y[validationIndex]
X_test <- remaining_X[-validationIndex, ]
y_test <- remaining_y[-validationIndex]

### Train the Polynomial Regression Model on the Training Set
# Separate the predictors for linear-only and polynomial terms
linear_terms_train <- X_train[, c('wage_eur', 'age', 'value_eur')]
poly_terms_train <- X_train[, c('pace', 'shooting', 'passing', 'dribbling', 
                                'defending', 'physic', 'skill_moves', 'weak_foot')]

# Generate polynomial features for only the selected predictors in the training set
poly_features_train <- as.data.frame(poly(as.matrix(poly_terms_train), degree = 2, raw = TRUE))

# Combine linear terms, polynomial features, and target variable
train_poly <- cbind(y_train, linear_terms_train, poly_features_train)
colnames(train_poly)[1] <- "overall"  # Rename target variable column

# Fit the polynomial regression model
poly_model1 <- lm(overall ~ ., data = train_poly)

# Summary of the polynomial model
poly_model_summary <- summary(poly_model1)
cat("Polynomial Regression Model Summary (Training):\n")
print(poly_model_summary)

### Validate the Model on the Validation Set
# Generate polynomial features for the validation set
linear_terms_val <- X_val[, c('wage_eur', 'age', 'value_eur')]
poly_terms_val <- X_val[, c('pace', 'shooting', 'passing', 'dribbling', 
                            'defending', 'physic', 'skill_moves', 'weak_foot')]
poly_features_val <- as.data.frame(poly(as.matrix(poly_terms_val), degree = 2, raw = TRUE))

# Combine validation linear terms, polynomial features, and target variable
val_poly <- cbind(y_val, linear_terms_val, poly_features_val)
colnames(val_poly)[1] <- "overall"

# Predict and calculate R-squared and MSE for the validation set
y_pred_val <- predict(poly_model1, newdata = val_poly)
r2_val <- 1 - sum((val_poly$overall - y_pred_val)^2) / sum((val_poly$overall - mean(val_poly$overall))^2)
mse_val <- mean((val_poly$overall - y_pred_val)^2)

# Print Validation R-squared and MSE
cat("\nValidation - Polynomial Regression R-squared:", r2_val, "\n")
cat("Validation - Polynomial Regression MSE:", mse_val, "\n")

### Evaluate the Model on the Test Set (Simulating External Validation)
# Generate polynomial features for the test set
linear_terms_test <- X_test[, c('wage_eur', 'age', 'value_eur')]
poly_terms_test <- X_test[, c('pace', 'shooting', 'passing', 'dribbling', 
                              'defending', 'physic', 'skill_moves', 'weak_foot')]
poly_features_test <- as.data.frame(poly(as.matrix(poly_terms_test), degree = 2, raw = TRUE))

# Combine test linear terms, polynomial features, and target variable
test_poly <- cbind(y_test, linear_terms_test, poly_features_test)
colnames(test_poly)[1] <- "overall"

# Predict and calculate R-squared and MSE for the test set
y_pred_test <- predict(poly_model1, newdata = test_poly)
r2_test <- 1 - sum((test_poly$overall - y_pred_test)^2) / sum((test_poly$overall - mean(test_poly$overall))^2)
mse_test <- mean((test_poly$overall - y_pred_test)^2)

# Print Test R-squared and MSE
cat("\nTest (Simulated External Validation) - Polynomial Regression R-squared:", r2_test, "\n")
cat("Test (Simulated External Validation) - Polynomial Regression MSE:", mse_test, "\n")

#####################
# cross validation
#####################

folds <- createFolds(y, k = 10, list = TRUE, returnTrain = TRUE)

# Initialize vectors to store MSE and R-squared for each fold
mse_folds <- c()
r2_folds <- c()

# Perform cross-validation
for (i in 1:10) {
  # Split data into training and test sets for the current fold
  train_index <- folds[[i]]
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Separate predictors and response for training set
  X_train <- train_data[, -1]
  y_train <- train_data$overall
  
  # Separate predictors and response for test set
  X_test <- test_data[, -1]
  y_test <- test_data$overall
  
  # Polynomial features for training set
  linear_terms_train <- X_train[, c('wage_eur', 'age', 'value_eur')]
  poly_terms_train <- X_train[, c('pace', 'shooting', 'passing', 'dribbling', 
                                  'defending', 'physic', 'skill_moves', 'weak_foot')]
  poly_features_train <- as.data.frame(poly(as.matrix(poly_terms_train), degree = 2, raw = TRUE))
  
  # Prepare training data for model
  train_poly <- cbind(y_train, linear_terms_train, poly_features_train)
  colnames(train_poly)[1] <- "overall"  # Rename target variable column
  
  # Fit the polynomial regression model
  poly_model_fold <- lm(overall ~ ., data = train_poly)
  
  # Polynomial features for test set
  linear_terms_test <- X_test[, c('wage_eur', 'age', 'value_eur')]
  poly_terms_test <- X_test[, c('pace', 'shooting', 'passing', 'dribbling', 
                                'defending', 'physic', 'skill_moves', 'weak_foot')]
  poly_features_test <- as.data.frame(poly(as.matrix(poly_terms_test), degree = 2, raw = TRUE))
  
  # Prepare test data for model
  test_poly <- cbind(y_test, linear_terms_test, poly_features_test)
  colnames(test_poly)[1] <- "overall"  # Rename target variable column
  
  # Make predictions on the test fold
  y_pred_fold <- predict(poly_model_fold, newdata = test_poly)
  
  # Calculate MSE and R-squared for the current fold
  mse_fold <- mean((y_test - y_pred_fold)^2)
  r2_fold <- 1 - sum((y_test - y_pred_fold)^2) / sum((y_test - mean(y_test))^2)
  
  # Store results for the fold
  mse_folds <- c(mse_folds, mse_fold)
  r2_folds <- c(r2_folds, r2_fold)
}

# Calculate average MSE and R-squared across all folds
mean_mse_cv <- mean(mse_folds)
mean_r2_cv <- mean(r2_folds)

# Print cross-validated MSE and R-squared
cat("Cross-validated Polynomial Regression Model Performance:\n")
cat("Mean MSE (10-fold CV):", mean_mse_cv, "\n")
cat("Mean R-squared (10-fold CV):", mean_r2_cv, "\n")

#############################
###### residual analysis ###
#############################
### Residual Analysis

# Calculate residuals for training, validation, and test sets
# Training residuals
y_pred_train <- predict(poly_model1, newdata = train_poly)
train_residuals <- train_poly$overall - y_pred_train

# Validation residuals
y_pred_val <- predict(poly_model1, newdata = val_poly)
val_residuals <- val_poly$overall - y_pred_val

# Test residuals
y_pred_test <- predict(poly_model1, newdata = test_poly)
test_residuals <- test_poly$overall - y_pred_test

### QQ Plots for Residuals

# Create individual QQ plots
qqplot_train <- ggplot(data = NULL, aes(sample = train_residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Training Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")

qqplot_val <- ggplot(data = NULL, aes(sample = val_residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Validation Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")

qqplot_test <- ggplot(data = NULL, aes(sample = test_residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Test Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Histograms to assess residual distribution
hist_train <- ggplot(data = NULL, aes(x = train_residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram of Training Residuals", x = "Residuals", y = "Density")

hist_val <- ggplot(data = NULL, aes(x = val_residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram of Validation Residuals", x = "Residuals", y = "Density")

hist_test <- ggplot(data = NULL, aes(x = test_residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Histogram of Test Residuals", x = "Residuals", y = "Density")

### Residuals vs Fitted Values Plot

# Fitted values for each dataset
fitted_train <- y_pred_train
fitted_val <- y_pred_val
fitted_test <- y_pred_test

# Residuals vs Fitted for Training
resid_vs_fitted_train <- ggplot(data = NULL, aes(x = fitted_train, y = train_residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values (Training)", x = "Fitted Values", y = "Residuals")

# Residuals vs Fitted for Validation
resid_vs_fitted_val <- ggplot(data = NULL, aes(x = fitted_val, y = val_residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values (Validation)", x = "Fitted Values", y = "Residuals")

# Residuals vs Fitted for Test
resid_vs_fitted_test <- ggplot(data = NULL, aes(x = fitted_test, y = test_residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values (Test)", x = "Fitted Values", y = "Residuals")

### Plot All Graphs Together

# Arrange QQ Plots
grid.arrange(qqplot_train, qqplot_val, qqplot_test, ncol = 3)

# Arrange Histograms
grid.arrange(hist_train, hist_val, hist_test, ncol = 3)

# Arrange Residuals vs Fitted Values Plots
grid.arrange(resid_vs_fitted_train, resid_vs_fitted_val, resid_vs_fitted_test, ncol = 3)

############
###PLOT######
##########

# Define R-squared and MSE values as named vectors
r_squared_values <- c(
  "Multiple Linear Regression" = 0.8018,
  "Polynomial Regression" =  0.9541,
  "Polynomial Regression (External Validation on Test)" = 0.9555,
  "Polynomial Regression (Cross-validation)" = 0.9538
)

mse_values <- c(
  "Multiple Linear Regression" = 8.91579,
  "Polynomial Regression" = 2.0603,
  "Polynomial Regression (External Validation on Test)" = 2.15144,
  "Polynomial Regression (Cross-validation)" = 2.075301
)

# Convert to data frames for ggplot
r_squared_df <- data.frame(Model = names(r_squared_values), R_Squared = r_squared_values)
mse_df <- data.frame(Model = names(mse_values), MSE = mse_values)

# Plot R-squared values with line and point chart
ggplot(r_squared_df, aes(x = Model, y = R_Squared, group = 1)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "R-squared Comparison Across Models", y = "R-squared", x = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot MSE values with line and point chart
ggplot(mse_df, aes(x = Model, y = MSE, group = 1)) +
  geom_line(color = "black", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "MSE Comparison Across Models", y = "Mean Squared Error (MSE)", x = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

