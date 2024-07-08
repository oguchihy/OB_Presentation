# Load necessary libraries
library(nnet)
library(dplyr)
library(caret)
library(glmnet)

# Load the one-hot encoded dataset
ob_data_encoded_m.nom <- readRDS("OB data encoded ALL VARIABLES for CoxPH.RDS")

# Inspect the dataset to understand its structure
str(ob_data_encoded_m.nom)

# Remove related variables
related_vars_events <- grep("intrapartal_events", names(ob_data_encoded_m.nom), value = TRUE)
data_events <- ob_data_encoded_m.nom %>% dplyr::select(-one_of(related_vars_events))

outcome_events <- 'intrapartal_events_None' # Replace with actual variable name if different

# Recode the outcome variable to model "yes" or not "None"
data_events$outcome_events <- ifelse(ob_data_encoded_m.nom[[outcome_events]] == 0, "None", "Yes")

# Convert outcome to factor
data_events$outcome_events <- as.factor(data_events$outcome_events)

# Remove predictors with zero variance
nzv <- nearZeroVar(data_events, saveMetrics = TRUE)
zero_var_predictors <- rownames(nzv[nzv$zeroVar, ])
data_events <- data_events %>% dplyr::select(-one_of(zero_var_predictors))

# Remove highly correlated predictors
correlation_matrix_events <- cor(data_events %>% dplyr::select(-outcome_events))
highly_correlated_events <- findCorrelation(correlation_matrix_events, cutoff = 0.8)
data_events <- data_events %>% dplyr::select(-one_of(names(data_events)[highly_correlated_events]))

# Prepare the data for glmnet
x_events <- model.matrix(outcome_events ~ ., data_events)[,-1]
y_events <- data_events$outcome_events

# Fit the regularized multinomial regression model (Ridge)
model_events <- cv.glmnet(x_events, y_events, family = "multinomial", alpha = 0)

# Extract coefficients for the best lambda
best_lambda <- model_events$lambda.min
coefficients_events <- coef(model_events, s = best_lambda)

# Print the coefficients for each class
print("Coefficients for intrapartal events:")
print(coefficients_events)
