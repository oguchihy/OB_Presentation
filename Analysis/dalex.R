# Load the packages
library(DALEX)
library(pdp)
library(iml)
library(randomForest)
library(ggplot2)

# Example model (assuming your data is in 'ob_data_filtered')
# Example model (assuming your data is in 'ob_data_filtered')
ob_data_filtered <- mdl.ds %>%
    filter(!is.na(delivery_birthweight_num2) & !is.na(age) & !is.na(gestational_days) &
               !is.na(city) & !is.na(zip) & !is.na(initial_trimester))

library(randomForest)
rf_model <- randomForest(delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester, data = ob_data_filtered)
rf_explainer <- DALEX::explain(rf_model, 
                               data = ob_data_filtered[, c("age", "gestational_days", "city", "zip", "initial_trimester")], 
                               y = ob_data_filtered$delivery_birthweight_num2)

# Calculate feature importance
fi <- model_parts(rf_explainer, type = "raw")

# Plot feature importance
plot(fi)

bd <- predict_parts(rf_explainer, new_observation = ob_data_filtered[1, ], type = "break_down")
plot(bd)

# This requires the installation of the 'fastshap' package for Random Forest models
library(fastshap)
# Define a prediction wrapper compatible with your Random Forest model
pred_wrapper <- function(model, newdata) {
    # Ensure newdata is a dataframe, as randomForest predict() expects a dataframe
    if (is.matrix(newdata)) {
        newdata <- as.data.frame(newdata)
    }
    
    # Assuming 'newdata' is already preprocessed (e.g., categorical variables are factors)
    predict(model, newdata)
}

# Ensure 'fastshap' is installed
if (!requireNamespace("fastshap", quietly = TRUE)) install.packages("fastshap")

library(fastshap)
X_prepared <- ob_data_filtered[, c("age", "gestational_days", "city", "zip", "initial_trimester")]

# Convert categorical variables to factors if your model was trained on data in that form
X_prepared$city <- as.factor(X_prepared$city)
X_prepared$zip <- as.factor(X_prepared$zip)

# Compute SHAP values
shap_values <- fastshap::explain(rf_model, X = X_prepared, pred_wrapper = pred_wrapper)

library(tidyverse)

# Assuming `shap_values` is your matrix/dataframe of SHAP values
# And `X_prepared` is the dataframe used for the prediction that corresponds to the SHAP values
shap_long <- as.data.frame(shap_values) %>%
    mutate(observation = row_number()) %>%
    pivot_longer(-observation, names_to = "feature", values_to = "shap_value")

# Adding actual feature values for each observation might provide more context in the plots
for (feature in names(X_prepared)) {
    shap_long <- shap_long %>%
        mutate(!!sym(feature) := X_prepared[observation, feature])
}

library(ggplot2)

# Assuming `shap_values` and `X_prepared` are already defined
# Convert SHAP values to a long-format dataframe
shap_long <- reshape2::melt(shap_values) %>%
    mutate(feature = factor(Var2, levels = unique(Var2)), # Ensuring features are factors for plotting
           observation = Var1) %>%
    select(-Var1, -Var2)

# Add actual 'age' values from `X_prepared` for plotting (assuming 'age' is in the correct order)
shap_long$age <- X_prepared$age[shap_long$observation]

# Plotting SHAP values for 'age'
ggplot(shap_long[shap_long$feature == "age", ], aes(x = age, y = value)) + # 'value' is the SHAP value from melt()
    geom_point(aes(color = value)) + # Coloring points by SHAP value
    scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) + # Color scale
    theme_minimal() +
    labs(title = "SHAP Values for Age", x = "Age", y = "SHAP Value")

##SHAP looping all variables
library(ggplot2)
library(dplyr)

plot_shap_for_feature <- function(feature_name, shap_data, feature_data) {
    # Determine if the feature is categorical or numerical
    feature_type <- if (is.numeric(feature_data[[feature_name]])) "numeric" else "categorical"
    
    # Extract relevant data for plotting
    plot_data <- shap_data %>%
        filter(feature == feature_name) %>%
        mutate(actual_value = feature_data[[feature_name]][observation])
    
    # Generate plot based on feature type
    if (feature_type == "numeric") {
        p <- ggplot(plot_data, aes(x = actual_value, y = value, color = value)) +
            geom_point() +
            scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
            theme_minimal() +
            labs(title = paste("SHAP Values for", feature_name), x = feature_name, y = "SHAP Value")
    } else { # For categorical features
        p <- ggplot(plot_data, aes(x = as.factor(actual_value), y = value, fill = value)) +
            geom_boxplot() +
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = paste("SHAP Values for", feature_name), x = feature_name, y = "SHAP Value")
    }
    
    return(p)
}
# For a numerical feature -- examples
p_numeric_gst_days <- plot_shap_for_feature("gestational_days", shap_long, X_prepared)
print(p_numeric_gst_days)

# For a numerical feature -- examples
p_numeric_age <- plot_shap_for_feature("age", shap_long, X_prepared)
print(p_numeric_age)

# For a categorical feature
p_categorical_city <- plot_shap_for_feature("city", shap_long, X_prepared)
print(p_categorical_city)

# For a categorical feature
p_categorical_zip <- plot_shap_for_feature("zip", shap_long, X_prepared)
print(p_categorical_zip)

# PDP for the 'age' feature
pdp_age <- model_profile(rf_explainer, variables = "age")
plot(pdp_age)

cp <- predict_profile(rf_explainer, new_observation = ob_data_filtered[1, ])
plot(cp)


# Model performance
mp <- model_performance(rf_explainer)
plot(mp)

# Residual diagnostics
rd <- model_diagnostics(rf_explainer)
plot(rd)


# Install and load vip

if (!requireNamespace("vip", quietly = TRUE)) install.packages("vip")
library(vip)

# Assuming 'model' is your regression model
vip(model)
