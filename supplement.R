#First, let's ensure your coefficients data includes the standard error values you need
if(!"Std. Error" %in% names(coef_data)) {
    # Assuming you might need to compute it or it's incorrectly named
print("Standard Error column missing or not found!")
# You might need to compute this or correct the reference based on your model output
} else {
    # Proceed if the column is present
    coef_data$CI_lower <- exp(coef_data[,"(Intercept)"] - 1.96 * coef_data$`Std. Error`)
    coef_data$CI_upper <- exp(coef_data[,"(Intercept)"] + 1.96 * coef_data$`Std. Error`)
    
    # Print to verify
    print(coef_data[c("OddsRatio", "CI_lower", "CI_upper")])
}

# Optionally, you might want to plot to visualize the odds ratios and confidence intervals
library(ggplot2)
ggplot(coef_data, aes(x = reorder(term, -OddsRatio), y = OddsRatio)) +
    geom_point() +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
    coord_flip() +  # Flips the axes for easier interpretation
    labs(x = "Variables", y = "Odds Ratio", title = "Logistic Regression Odds Ratios and 95% Confidence Intervals") +
    theme_minimal()


###
# # Add a 'term' column for the variable names
# coef_data$term <- rownames(coef_data)
# 
# # Plotting the Odds Ratios using ggplot2
# library(ggplot2)
# ggplot(coef_data, aes(x = reorder(term, -OddsRatio), y = OddsRatio)) +
#     geom_point() +
#     geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
#     coord_flip() +  # Flips the axes for easier interpretation
#     labs(x = "Variables", y = "Odds Ratio", title = "Logistic Regression Odds Ratios and 95% Confidence Intervals") +
#     theme_minimal()
# 
# # Assuming 'Std. Error' is the standard error for each coefficient
# coef_data$OddsRatio <- exp(coef_data[,"(Intercept)"])
# coef_data$CI_lower <- exp(coef_data[,"(Intercept)"] - 1.96 * coef_data[,"Std. Error"])
# coef_data$CI_upper <- exp(coef_data[,"(Intercept)"] + 1.96 * coef_data[,"Std. Error"])


# Fit a logistic regression model
model <- glm(event ~ age + grav + para + gest_age_days + intrapartal_conditions + intrapartal_events,
             family = binomial(link = "logit"),
             data = ob_data_mdl)

# Extract coefficients from the model
coef_summary <- summary(model)$coefficients

# Create a dataframe for coefficients, Odds Ratios, and Confidence Intervals
coef_data <- data.frame(
    Estimate = coef_summary[, "Estimate"],
    StdError = coef_summary[, "Std. Error"],
    zValue = coef_summary[, "z value"],
    PValue = coef_summary[, "Pr(>|z|)"],
    OddsRatio = exp(coef_summary[, "Estimate"]),
    CI_lower = exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"]),
    CI_upper = exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"])
)

# Plot using ggplot2
library(ggplot2)
ggplot(coef_data, aes(x = reorder(names(coef_data$Estimate), -OddsRatio), y = OddsRatio)) +
    geom_point() +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
    coord_flip() +  # Flips the axes for easier interpretation
    labs(x = "Variables", y = "Odds Ratio", title = "Logistic Regression Odds Ratios and 95% Confidence Intervals") +
    theme_minimal()
