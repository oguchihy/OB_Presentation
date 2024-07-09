# Load the packages
library(DALEX)
library(pdp)
library(iml)
library(randomForest)
library(ggplot2)

# Load or create the filtered data
ob_data_filtered <- readRDS("ob_data_filtered.RDS")

# Train the Random Forest model
rf_model <- randomForest(delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester +uds_age, data = ob_data_filtered)

# Save the trained model
saveRDS(rf_model, "rf_model.RDS")

# Create explainer object
rf_explainer <- DALEX::explain(rf_model, 
                               data = ob_data_filtered[, c("age", "gestational_days", "city", "zip", "initial_trimester", "uds_age")], 
                               y = ob_data_filtered$delivery_birthweight_num2)

# Save the explainer object
saveRDS(rf_explainer, "rf_explainer.RDS")

# Calculate feature importance
fi <- model_parts(rf_explainer, type = "raw")

# Plot feature importance
plot(fi)

# Obtain prediction breakdown for a sample observation
bd <- predict_parts(rf_explainer, new_observation = ob_data_filtered[1, ], type = "break_down")
plot(bd)

# Ensure 'fastshap' is installed
if (!requireNamespace("fastshap", quietly = TRUE)) install.packages("fastshap")

# Define a prediction wrapper compatible with your Random Forest model
pred_wrapper <- function(model, newdata) {
    # Ensure newdata is a dataframe, as randomForest predict() expects a dataframe
    if (is.matrix(newdata)) {
        newdata <- as.data.frame(newdata)
    }
    
    # Assuming 'newdata' is already preprocessed (e.g., categorical variables are factors)
    predict(model, newdata)
}

# Load the prepared data
X_prepared <- readRDS("X_prepared.RDS")

# Compute SHAP values
shap_values <- fastshap::explain(rf_model, X = X_prepared, pred_wrapper = pred_wrapper)

