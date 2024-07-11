library(poLCA)

library(poLCA)

# Converting categorical predictors to factor type
ob_data_compact_qrto$gender <- as.factor(ob_data_compact_qrto$gender)
ob_data_compact_qrto$lbr_type_cnsldt <- as.factor(ob_data_compact_qrto$lbr_type_cnsldt)
ob_data_compact_qrto$presentation_cnsldt <- as.factor(ob_data_compact_qrto$presentation_cnsldt)

# Convert outcome variables to factors if they are not already
ob_data_compact_qrto$conditions_cnsldt <- as.factor(ob_data_compact_qrto$conditions_cnsldt)
ob_data_compact_qrto$intrapartal_events <- as.factor(ob_data_compact_qrto$intrapartal_events)
ob_data_compact_qrto$gender <- as.factor(ob_data_compact_qrto$gender)

# Check the structure to confirm changes
str(ob_data_compact_qrto[c("conditions_cnsldt", "intrapartal_events")])


# Define the formula for the LCA model, using both outcomes and predictors
lca_formula <- formula(cbind(conditions_cnsldt, intrapartal_events) ~ age + gest_age_days + weight + gender + diff_grav_para + lbr_type_cnsldt + presentation_cnsldt, data = ob_data_compact_qrto)

# Fit the LCA model, adjusting 'nclass' as needed based on model fit and theoretical considerations
lca_model <- poLCA(lca_formula, data = ob_data_compact_qrto, nclass = 3, maxiter = 1000, tol = 1e-6)

# Summary of the model
summary(lca_model)

# Plot the class probabilities and conditional probabilities of the responses
plot(lca_model, what = "prob")

# Summary of the model
summary(lca_model)

# Plot the class probabilities and conditional probabilities of the responses
plot(lca_model, what = "prob")


# Define the formula for the LCA model
# Here, cbind creates a matrix of responses (conditions_cnsldt and intrapartal_events),
# which poLCA uses to model the latent classes
lca_formula <- formula(cbind(conditions_cnsldt, intrapartal_events) ~ age + gest_age_days + weight + gender + gest_age_days + diff_grav_para + 
                           lbr_type_cnsldt + presentation_cnsldt, data = ob_data_compact_qrto)

# Fit the LCA model
# nclass specifies the number of latent classes you hypothesize (this can require some trial and error or model fit analysis)
lca_model <- poLCA(lca_formula, data = ob_data_compact_qrto, nclass = 3)

# Summary of the model
summary(lca_model)

# Plot the probability of class membership and conditional item response probabilities
plot(lca_model)


# Plotting class probabilities
barplot(lca_model$P, main="Class Membership Probabilities", col=rainbow(length(lca_model$P)))

# Plotting response probabilities for a selected class
barplot(unlist(lca_model$probs[1]), main="Response Probabilities for Class 1", beside=TRUE, col=rainbow(length(lca_model$probs[[1]])))

# Plotting coefficients
barplot(lca_model$coeff, main="Predictor Coefficients", beside=TRUE, col=rainbow(length(lca_model$coeff)))


# Assuming 'lca_model' is your fitted model
coefficients <- lca_model$coeff
# Assuming you have labels for each predictor as they appear in your model
predictor_labels <- c("Age", "Gestational Age Days", "Weight", "Gender", "Differential Gravida-Para", "Labor Type Consolidated", "Presentation Consolidated")

# Match the coefficients with labels
# Assuming 'coefficients' is a matrix or data frame with each row corresponding to a class and each column to a predictor
# Recreate the labeled_coefficients dataframe correctly
# Assuming 'coefficients' is your raw coefficient matrix directly from the model
# Transpose coefficients for proper orientation
coefficients_transposed <- t(coefficients)

# # Create a dataframe with proper labeling
# # Assuming there are 2 classes (adjust this number if it's different)
# num_classes <- 2
# predictor_labels <- c("(Intercept)", "Age", "Gestational Age Days", "Weight", "Gender Male", "Gender Unknown", 
#                       "Diff Grav Para", "Labor Type Induced", "Labor Type Not Applicable", "Labor Type Spontaneous",
#                       "Labor Type Spontaneous, Augmented", "Labor Type Suppressed", "Presentation Vertex")
# 
# # Reconstruct the dataframe with separate columns for each class
# labeled_coefficients <- as.data.frame(matrix(as.numeric(unlist(labeled_coefficients)), nrow = num_classes, byrow = TRUE))
# names(labeled_coefficients) <- predictor_labels
# 
# # Display corrected structure
# print(labeled_coefficients)
# 
# # Assuming `labeled_coefficients` has been corrected as per above setup
# # Create a long format data frame with class labels
# library(tidyr)
# long_coefficients <- pivot_longer(labeled_coefficients, cols = everything(),
#                                   names_to = "Predictor", values_to = "Coefficient",
#                                   names_pattern = "(.*)")
# 
# # Add a class identifier based on row appearance (every predictor repeats for each class)
# long_coefficients$Class <- rep(1:num_classes, each = nrow(labeled_coefficients) * length(predictor_labels) / num_classes)
# 
# # Display the structure to check
# print(long_coefficients)
# 
# 
# library(ggplot2)
# 
# # Plotting the coefficients
# ggplot(long_coefficients, aes(x = Predictor, y = Coefficient, fill = as.factor(Class))) +
#     geom_bar(stat = "identity", position = position_dodge()) +
#     theme_minimal() +
#     labs(title = "Predictor Coefficients by Latent Class",
#          x = "Predictor", y = "Coefficient") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ###


# Assuming there are 2 classes and predictors are already correct in your output
num_classes <- 2
predictor_labels <- c("(Intercept)", "Age", "Gestational Age Days", "Weight", "Gender Male", "Gender Unknown", 
                      "Diff Grav Para", "Labor Type Induced", "Labor Type Not Applicable", "Labor Type Spontaneous",
                      "Labor Type Spontaneous, Augmented", "Labor Type Suppressed", "Presentation Vertex")

# Since it appears you have 2 rows of coefficients for each class
class_labels <- paste0("Class ", rep(1:num_classes, each = length(predictor_labels)))

# Create a new long format dataframe suitable for ggplot
long_coefficients <- data.frame(
    Class = rep(class_labels, times = 2),
    Predictor = rep(predictor_labels, each = num_classes),
    Coefficient = as.numeric(t(labeled_coefficients))
)

# Check the structure
print(long_coefficients)

library(ggplot2)

# Plotting the coefficients
ggplot(long_coefficients, aes(x = Predictor, y = Coefficient, fill = as.factor(Class))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    labs(title = "Predictor Coefficients by Latent Class",
         x = "Predictor", y = "Coefficient") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


