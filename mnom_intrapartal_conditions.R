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
