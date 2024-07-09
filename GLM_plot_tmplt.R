library(readr)
library(openxlsx)
library(xlsx)
library(DataExplorer)
library(explore)
library(XICOR)
library(survival)
library(survminer)
library(klaR)
library(ggforestplot)
library(forestplot) 
library(myRFunctions)
library(mice)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(randomForest)
library(leaflet)
library(sf)
library(tableone)
library(DT)
library(kableExtra)

library(survminer)
library(survival)
library(ggplot2)


ob_data_fctr <- readRDS("All Factored Complete Ready OB Dataset for Analytics.RDS")

# Derive the binary outcome from the cluster variable
ob_data_fctr$cluster <- ifelse(ob_data_fctr$cluster == 2, 1, 0)

# Convert the binary outcome to a factor
ob_data_fctr$binary_outcome <- as.factor(ob_data_fctr$cluster)

# Calculate the frequency of each zip code
zip_freq <- table(ob_data_fctr$zip)

# Define the frequency threshold
threshold <- 60

# Create a new column categorizing zip codes based on the threshold
ob_data_fctr$zip_category <- ifelse(zip_freq[ob_data_fctr$zip] >= threshold,
                                    as.character(ob_data_fctr$zip),
                                    "Other")

# Convert the new zip_category column to a factor
ob_data_fctr$zip_category <- as.factor(ob_data_fctr$zip_category)

# Fit the logistic regression model with the specified predictor variables
suppressWarnings({
  model <- glm(binary_outcome ~ zip_category + age + grav + para + gender + uds_age + hghRsk + del_method_cnsldt + lbr_type_cnsldt, 
               data = ob_data_fctr, family = binomial())
})



# Extract coefficients, confidence intervals, and p-values
coef_summary <- summary(model)$coefficients
conf_int <- confint(model)

# Combine results into a single data frame
results <- as.data.frame(coef_summary)
results$conf.low <- conf_int[, 1]
results$conf.high <- conf_int[, 2]

# Add predictor names
results$predictor <- rownames(results)
rownames(results) <- NULL

# Convert log-odds to odds ratios
results <- results %>%
  mutate(OR = exp(Estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high),
         p.value = `Pr(>|z|)`)

# Remove rows with missing values
results <- results[complete.cases(results), ]

# Identify reference levels
reference_levels <- c(
  "zip_categoryOther", 
  "genderFemale", 
  "uds_age15-19", 
  "hghRskno", 
  "del_method_cnsldtC/S, Primary", 
  "lbr_type_cnsldtSpontaneous"
)

library(ggplot2)

# Plotting
ggplot(results, aes(x = reorder(predictor, OR), y = OR)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_text(aes(label = sprintf("p = %.3f", p.value), color = ifelse(p.value < 0.05, "red", "black")), 
            hjust = -0.3, size = 3) +
  geom_text(aes(label = sprintf("OR = %.2f", OR)), vjust = -1.5, size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +  # Add a line at OR = 1 for reference
  scale_y_log10(limits = c(0.5, 10)) +  # Start y-axis from 0.5
  labs(title = "Odds Ratios and 95% Confidence Intervals from GLM",
       x = "Predictor",
       y = "Odds Ratio (log scale)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        plot.caption = element_text(size = 10, hjust = 1, vjust = 1, face = "italic")) +
  coord_flip() +  # Flip coordinates for better readability
  annotate("text", x = Inf, y = Inf, label = "Reference Levels:\nzip_category: Other\nGender: Female\nuds_age: 15-19\nhghRsk: No\nDel Method: C/S, Primary\nLabor Type: Spontaneous",
           hjust = 1.2, vjust = 1.2, size = 4, color = "black", alpha = 0.6, fontface = "italic", angle = 0)
