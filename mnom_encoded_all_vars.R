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

##Intrapartal events

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



### # Extract coefficients for the best lambda
coefficients_events <- coef(model_events, s = best_lambda)

# Convert to a matrix for easier manipulation
coefficients_matrix_events <- as.matrix(coefficients_events[[1]])

# Print the coefficients
print("Coefficients for intrapartal events:")
print(coefficients_matrix_events)

# Identify and print the most important coefficients (magnitude-based)
important_coefficients_events <- coefficients_matrix_events[abs(coefficients_matrix_events) > 0]
print("Important coefficients for intrapartal events (magnitude-based):")
print(important_coefficients_events)


# Load necessary library
library(ggplot2)

# Load necessary library
library(ggplot2)
library(dplyr)
library(tidyr)

# Extract coefficients for all classes
coefficients_list_events <- coef(model_events, s = best_lambda)

# Combine the coefficients into a single data frame
coefficients_df_events <- do.call(rbind, lapply(seq_along(coefficients_list_events), function(i) {
  coefs <- as.matrix(coefficients_list_events[[i]])
  df <- as.data.frame(coefs)
  df$Class <- names(coefficients_list_events)[i]
  df$Predictor <- rownames(df)
  df
}))

# Melt the data frame for ggplot2
coefficients_melted_events <- coefficients_df_events %>%
  pivot_longer(cols = -c(Class, Predictor), names_to = "CoefficientType", values_to = "Coefficient")

# Plot the coefficients
ggplot(coefficients_melted_events, aes(x = reorder(Predictor, Coefficient), y = Coefficient, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Coefficient, 2)), position = position_dodge(width = 0.9), hjust = -0.2) +
  coord_flip() +
  labs(title = "Coefficients for Intrapartal Events",
       x = "Predictor",
       y = "Coefficient Value") +
  theme_minimal() +
  theme(legend.position = "bottom")





##Intrapartal conditions

# Remove related variables
related_vars_conditions <- grep("conditions|time", names(ob_data_encoded_m.nom), value = TRUE)
data_conditions <- ob_data_encoded_m.nom %>% dplyr::select(-one_of(related_vars_conditions))

outcome_conditions <- 'conditions_cnsldt_None' # Replace with actual variable name if different

# Recode the outcome variable to model "yes" or not "None"
data_conditions$outcome_conditions <- ifelse(ob_data_encoded_m.nom[[outcome_conditions]] == 0, "None", "Yes")

# Convert outcome to factor
data_conditions$outcome_conditions <- as.factor(data_conditions$outcome_conditions)

# Remove predictors with zero variance
nzv_conditions <- nearZeroVar(data_conditions, saveMetrics = TRUE)
zero_var_predictors_conditions <- rownames(nzv_conditions[nzv_conditions$zeroVar, ])
data_conditions <- data_conditions %>% dplyr::select(-one_of(zero_var_predictors_conditions))

# Remove highly correlated predictors
correlation_matrix_conditions <- cor(data_conditions %>% dplyr::select(-outcome_conditions))
highly_correlated_conditions <- findCorrelation(correlation_matrix_conditions, cutoff = 0.8)
data_conditions <- data_conditions %>% dplyr::select(-one_of(names(data_conditions)[highly_correlated_conditions]))

# Prepare the data for glmnet
x_conditions <- model.matrix(outcome_conditions ~ ., data_conditions)[,-1]
y_conditions <- data_conditions$outcome_conditions

# Fit the regularized multinomial regression model (Ridge)
model_conditions <- cv.glmnet(x_conditions, y_conditions, family = "multinomial", alpha = 0)

# Extract coefficients for the best lambda
best_lambda_conditions <- model_conditions$lambda.min
coefficients_conditions <- coef(model_conditions, s = best_lambda_conditions)

# Print the coefficients for each class
print("Coefficients for intrapartal conditions:")
print(coefficients_conditions)



### # Extract coefficients for all classes
coefficients_list_conditions <- coef(model_conditions, s = best_lambda_conditions)

# Combine the coefficients into a single data frame
coefficients_df_conditions <- do.call(rbind, lapply(seq_along(coefficients_list_conditions), function(i) {
  coefs <- as.matrix(coefficients_list_conditions[[i]])
  df <- as.data.frame(coefs)
  df$Class <- names(coefficients_list_conditions)[i]
  df$Predictor <- rownames(df)
  df
}))

# Melt the data frame for ggplot2
coefficients_melted_conditions <- coefficients_df_conditions %>%
  pivot_longer(cols = -c(Class, Predictor), names_to = "CoefficientType", values_to = "Coefficient")

# Plot the coefficients
ggplot(coefficients_melted_conditions, aes(x = reorder(Predictor, Coefficient), y = Coefficient, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Coefficient, 2)), position = position_dodge(width = 0.9), hjust = -0.2) +
  coord_flip() +
  labs(title = "Coefficients for Intrapartal Conditions",
       x = "Predictor",
       y = "Coefficient Value") +
  theme_minimal() +
  theme(legend.position = "bottom")
