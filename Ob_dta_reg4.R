#setwd("C:/Users/onkwocha/Dropbox/CSVS/OB Service/NMC OB LOG")
# Build the package path using the home directory
# package_path <- file.path("~", "Dropbox", "myRFunctions_1.0.tar.gz")
# 
# # Normalize the path to correctly resolve the ~ on Windows
# package_path <- normalizePath(package_path, winslash = "/")
# 
# # Install the package from the specified source file
#package_path <- "C:/Users/oguch/Dropbox/myRFunctions"
# install.packages(package_path, repos = NULL, type = "source")

library(tidyverse)
library(dplyr)
library(dtplyr)
library(lubridate)
library(anytime)
library(janitor)
library(ggplot2)
library(readxl)
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


# Assuming your data frame is named ob_data_ans and you've selected some numeric columns
# Standardizing the data
ob_data_reg <- readRDS("OB DATA no NAs.RDS")
#install.packages("mice")

#Remove uneeded vars source of NAs----
ob_data_slim <- ob_data_reg 
ob_data_slim$adm_datetime <- NULL
ob_data_slim$delivery_datetime <- NULL
ob_data_slim$adm_time <- NULL
ob_data_slim$delivery_time <- NULL
ob_data_slim$gender[is.na(ob_data_slim$gender)] <- "unknown"


# Check for NAs in each column
colSums(is.na(ob_data_slim))

# Impute missing values using the median for numerical variables and the mode for categorical variables
# This is a basic method, and you might consider more sophisticated imputation methods based on your needs
library(dplyr)

# Numerical columns imputation with median
numerical_cols <- sapply(ob_data_slim, is.numeric)
ob_data_slim[numerical_cols] <- lapply(ob_data_slim[numerical_cols], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Categorical columns imputation with mode
categorical_cols <- sapply(ob_data_slim, is.character)
ob_data_slim[categorical_cols] <- lapply(ob_data_slim[categorical_cols], function(x) ifelse(is.na(x), names(which.max(table(x))), x))
colSums(is.na(ob_data_slim))

saveRDS(ob_data_slim, "Cleaned OB Dataset Ready for Analysis.RDS")

ob_data_mdl <- readRDS("Cleaned OB Dataset Ready for Analysis.RDS")

#Random Forest ----

library(randomForest)

ob_data_mdl$high_risk <- factor(if_else(ob_data_mdl$adm_to_del_tm > 48, 1, 0), levels = c(0, 1))

# Assuming 'high_risk' is a binary outcome you're interested in predicting
# Make sure 'high_risk' is properly formatted as a factor if it's a classification task
#ob_data_mdl$high_risk <- factor(ob_data_mdl$high_risk, levels = c("0", "1"))

# Fit Random Forest model
set.seed(123)  # for reproducibility
rf_model <- randomForest(high_risk ~ ., data = ob_data_mdl, ntree = 100)

# Check model accuracy
print(rf_model)

# View variable importance
importance(rf_model)
varImpPlot(rf_model)

#Survival Analysis with surminer ----

library(survival)
library(survminer)

# Assuming 'event' and 'time' are properly defined in your dataset
# For this example, let's say we are using 'adm_to_del_tm' as time and creating a binary 'event'
ob_data_mdl$event <- ifelse(!is.na(ob_data_mdl$delivery_date), 1, 0) # Adjust condition based on your definition of an event

# Create the survival object
surv_obj <- Surv(time = ob_data_mdl$adm_to_del_tm, event = ob_data_mdl$event)

# Fit Kaplan-Meier survival curve
km_fit <- survfit(surv_obj ~ 1, data = ob_data_mdl)

# Plot the survival curve
ggsurvplot(km_fit, data = ob_data_mdl,
           xlab = "Time from Admission to Delivery (hours)",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curve",
           ggtheme = theme_minimal())


# Fit the Cox model----
cox_model <- coxph(surv_obj ~ age + grav + para + gest_age_days +intrapartal_conditions + intrapartal_events,
                   data = ob_data_mdl)

# Summary of the Cox model
summary(cox_model)


# Install and load ggforestplot if not already installed
if (!require("ggforestplot")) {
    devtools::install_github("NightingaleHealth/ggforestplot")
    library(ggforestplot)
}

# Install the broom package if not already installed
if (!require("broom")) {
    install.packages("broom")
    library(broom)
}

# Now tidy your Cox model ----
tidy_cox <- tidy(cox_model, exponentiate = TRUE)  # Getting exponentiated coefficients for HRs

# Check the structure and contents of tidy_cox
str(tidy_cox)
head(tidy_cox)

# Ensure that the 'term' column is correctly factored and ordered
tidy_cox$term <- factor(tidy_cox$term, levels = unique(tidy_cox$term))


# Check the structure and contents of tidy_cox
str(tidy_cox)
head(tidy_cox)

# Ensure that the 'term' column is correctly factored and ordered
tidy_cox$term <- factor(tidy_cox$term, levels = unique(tidy_cox$term))

# Convert 'term' to a factor
tidy_cox$term <- factor(tidy_cox$term, levels = unique(tidy_cox$term))

# Plot using ggplot2 to check the setup
library(ggplot2)
ggplot(tidy_cox, aes(x = reorder(term, estimate), y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
    coord_flip() +
    labs(x = "Term", y = "Hazard Ratio (HR)") +
    theme_minimal()

# Ensure 'term' is a factor and check for unique values directly
tidy_cox$term <- factor(tidy_cox$term, levels = unique(tidy_cox$term))

# Try a basic forest plot with manual control using ggplot2
library(ggplot2)
ggplot(tidy_cox, aes(y = term, x = estimate, xmin = estimate - std.error, xmax = estimate + std.error)) +
    geom_point() +
    geom_errorbarh(height = 0.2) +
    theme_minimal() +
    labs(x = "Hazard Ratio", y = "Variable")

# If this works without error, you might consider skipping ggforestplot if it continues to fail
library(ggplot2)

# Generate the plot with error bars and points
plot <- ggplot(tidy_cox, aes(y = term, x = estimate, xmin = estimate - std.error, xmax = estimate + std.error)) +
    geom_point() +
    geom_errorbarh(height = 0.2) +
    theme_minimal() +
    labs(x = "Hazard Ratio", y = "Variable")

# Add text annotations for HR values and p-values
plot <- plot + 
    geom_text(aes(label = sprintf("HR=%.2f, p=%.3f", estimate, p.value), x = estimate + std.error + 0.02), 
              hjust = 0, vjust = 0.5, size = 3, color = "blue")

# Display the plot
plot

#Cox ----
# Load necessary libraries
library(survival)
library(survminer)

# Make sure your data frame is read correctly
ob_data_mdl <- readRDS("Cleaned OB Dataset Ready for Analysis.RDS")

# Convert the 'time' variable to numeric if it's not already
ob_data_mdl$adm_to_del_tm <- as.numeric(ob_data_mdl$adm_to_del_tm)

# Assuming you define an 'event' variable or it already exists and is properly coded
# For example, you might define an event as a particular condition being met
# ob_data_mdl$event <- ifelse(condition, 1, 0)

# Check if 'event' variable is present and correct
if(!"event" %in% names(ob_data_mdl)) {
    ob_data_mdl$event <- ifelse(!is.na(ob_data_mdl$adm_to_del_tm), 1, 0)  # Example condition
}

# Fit the Cox proportional hazards model 
cox_model <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days 
                   + intrapartal_conditions + intrapartal_events, data = ob_data_mdl)


###

library(survival)
library(survminer)
# With ob_dat_amdl
# Assuming 'ob_data_mdl' is loaded and prepared
# Ensure that 'event' and 'adm_to_del_tm' are correctly formatted and present
if(!"event" %in% names(ob_data_mdl) || !is.numeric(ob_data_mdl$adm_to_del_tm)) {
    ob_data_mdl$adm_to_del_tm <- as.numeric(ob_data_mdl$adm_to_del_tm)
    ob_data_mdl$event <- ifelse(!is.na(ob_data_mdl$adm_to_del_tm), 1, 0)  # Assuming event is defined like this
}

# Fit the Cox proportional hazards model----
cox_model <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days + weight + diff_grav_para + intrapartal_conditions + intrapartal_events,
                   data = ob_data_mdl)

# Use survminer's ggforest to visualize the model's output
ggforest(cox_model, data = ob_data_mdl)


##Consolidate intrapartalconditions and events ----

ob_data_cnsldt <- ob_data_mdl %>% 
    mutate(
        conditions_cnsldt = case_when(
            str_detect(intrapartal_conditions, "Chorio") | str_detect(intrapartal_conditions, "Strep") & !str_detect(intrapartal_conditions, "Preecl")~"infection",
            str_detect(intrapartal_conditions, "Preeclampsia") & !(str_detect(intrapartal_conditions, "Chorio") | str_detect(intrapartal_conditions, "Strep"))~"Preeclampsia",
            str_detect(intrapartal_conditions, "Preeclampsia") & (str_detect(intrapartal_conditions, "Chorio") | str_detect(intrapartal_conditions, "Strep"))~"Preeclampsia_Infection",
            str_detect(intrapartal_conditions, "Prolo") & !str_detect(intrapartal_conditions, ",") ~ "Prolonged ROM",
            str_detect(intrapartal_conditions, "Abrupt") & !(str_detect(intrapartal_conditions, "Prolon") | str_detect(intrapartal_conditions, "Strep"))~"Abruption Placenta",
            TRUE ~ as.character(intrapartal_conditions)
        ),
          ) %>% 
    mutate(conditions_cnsldt = if_else(conditions_cnsldt =="Prolonged ROM, Abruption Placenta, Hemorrhage", "Abruption Placenta", 
                                       if_else(conditions_cnsldt == "Prolonged ROM, Hemorrhage", "Prolonged ROM", conditions_cnsldt)))

saveRDS(ob_data_cnsldt, "OB Data with consolidated Intrapartal conditions.RDS")


##Consolidate labor_type and presentation ----

ob_data_cnsldt_more <- ob_data_cnsldt %>% 
    mutate(
        lbr_type_cnsldt = case_when(
            str_detect(labor_type, "pplicable") ~ "Not Applicable",
            #str_detect(labor_type, "Induced, Not Applicable") ~ "Not Applicable",
            str_detect(labor_type, "uppressed") ~ "Suppressed",
            str_detect(labor_type, "Spontaneous, Induced") ~ "Not Applicable",
            str_detect(labor_type, "nduced, Augmented") ~ "Induced",
            #str_detect(labor_type, "Spontaneous, Not Applicable") ~ "Not Applicable",
            #str_detect(labor_type, ", Not Applicable") ~ "Not Applicable",
            #str_detect(labor_type, ", Suppressed") ~ "Not Applicable",
            TRUE ~ as.character(labor_type)
        
        ),
        presentation_cnsldt=if_else(presentation == "Vertex", "Vertex", "Breech"),
        lbr_type_cnsldt = if_else(lbr_type_cnsldt == "Induced, Suppressed", "Suppresses",lbr_type_cnsldt),
        lbr_type_cnsldt = if_else(lbr_type_cnsldt == "Spontaneous, Induced", "Not Applicable",lbr_type_cnsldt),
        lbr_type_cnsldt = if_else(lbr_type_cnsldt == "Spontaneous, Not Applicable", "Not Applicable",lbr_type_cnsldt),
        lbr_type_cnsldt = if_else(lbr_type_cnsldt == "Induced, Not Applicable", "Not Applicable",lbr_type_cnsldt)
    )

saveRDS(ob_data_cnsldt_more, "OB Data with consolidated Intrapartal conditions, labor type and presentation.RDS")

#Rerun Cox model with consolidated vars----

library(survival)
library(survminer)

# Load the dataset
ob_data_compact <- readRDS("OB Data with consolidated Intrapartal conditions, labor type and presentation.RDS")

# Check and prepare variables
if(!"event" %in% names(ob_data_compact) || !is.numeric(ob_data_compact$adm_to_del_tm)) {
    ob_data_compact$adm_to_del_tm <- as.numeric(ob_data_compact$adm_to_del_tm)
    ob_data_mdl$event <- ifelse(!is.na(ob_data_mdl$delivery_date), 1, 0)  # Assuming event is defined like this
}
#1. Intrapartal EVENTS----

# Convert intrapartal_events to a factor and adjust the reference level to 'None'
ob_data_compact$intrapartal_events <- factor(ob_data_compact$intrapartal_events)
ob_data_compact$intrapartal_events <- relevel(ob_data_compact$intrapartal_events, ref = "None")

# Labor type and presentation
ob_data_compact$lbr_type_cnsldt <- factor(ob_data_compact$lbr_type_cnsldt)
ob_data_compact$lbr_type_cnsldt <- relevel(ob_data_compact$lbr_type_cnsldt, ref = "Spontaneous")

ob_data_compact$presentation_cnsldt <- factor(ob_data_compact$presentation_cnsldt)
ob_data_compact$presentation_cnsldt <- relevel(ob_data_compact$presentation_cnsldt, ref = "Vertex")


# Fit the Cox proportional hazards model with adjusted reference level
cox_model <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days + weight + presentation_cnsldt + lbr_type_cnsldt + diff_grav_para + intrapartal_events,
                   data = ob_data_compact)

# Use survminer's ggforest to visualize the model's output
ggforest(cox_model, data = ob_data_compact)



# Intrapartal conditions----
# Convert intrapartal_events to a factor and adjust the reference level to 'None'
ob_data_compact$conditions_cnsldt <- factor(ob_data_compact$conditions_cnsldt)
ob_data_compact$conditions_cnsldt <- relevel(ob_data_compact$conditions_cnsldt, ref = "None")

# Labor type and presentation
ob_data_compact$lbr_type_cnsldt <- factor(ob_data_compact$lbr_type_cnsldt)
ob_data_compact$lbr_type_cnsldt <- relevel(ob_data_compact$lbr_type_cnsldt, ref = "Spontaneous")

ob_data_compact$presentation_cnsldt <- factor(ob_data_compact$presentation_cnsldt)
ob_data_compact$presentation_cnsldt <- relevel(ob_data_compact$presentation_cnsldt, ref = "Vertex")

# Fit the Cox proportional hazards model with adjusted reference level
cox_model <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days + weight + presentation_cnsldt + lbr_type_cnsldt +diff_grav_para + conditions_cnsldt,
                   data = ob_data_compact)

# Use survminer's ggforest to visualize the model's output
library(survival)
library(survminer)
library(ggplot2)
library(broom)
#setwd(getwd())
#install.packages("C:/Users/onkwocha/Dropbox/myRFunctions", repos = NULL, type = "source")
library(myRFunctions)


ggforest3(cox_model, data = ob_data_compact)



#Cox Fit Curve, cumulative hazard and cumulative event ----
library("survival")
library("survminer")
ob_data_compact$gender <- factor(ob_data_compact$gender, levels = c("Female", "Male", "unknown"))

# Now, fit the Cox proportional hazards model
fit <- coxph(Surv(adm_to_del_tm, event) ~ gender + age + grav + para + gest_age_days + weight + diff_grav_para + conditions_cnsldt,
             data = ob_data_compact)
surv_adjustedcurves(fit, data=ob_data_compact)
ggadjustedcurves(fit, data=ob_data_compact,variable = "gender")


# Compute survival probabilities
# surv_fit <- survfit(fit)
# 
# # Plot cumulative hazard using survminer
# ggsurvplot(surv_fit, data = ob_data_compact, fun = "cumhaz")
# 
# ggsurvplot(fit, data=ob_data_compact, fun = "cumhaz")


library(survival)
library(survminer)

# Fit Cox model
fit <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days + weight + diff_grav_para + conditions_cnsldt,
             data = ob_data_compact)

# Compute Kaplan-Meier survival estimate
surv_fit <- survfit(fit)

# Plot cumulative survival over time
ggsurvplot(surv_fit, data = ob_data_compact, conf.int = FALSE, risk.table = TRUE, fun = "event")

#Regression----
library(stats)
library(broom)

#Ensure the dataset is loaded
ob_data_logit <- ob_data_compact

# If 'high_risk' is not already in your dataset, create it for demonstration purposes
# This is a placeholder: you should replace it with your actual method of defining high risk
if(!"high_risk" %in% names(ob_data_logit)) {
    ob_data_logit$high_risk <- ifelse(ob_data_logit$adm_to_del_tm > 48, 1, 0)
}

# Convert high_risk to a factor for logistic regression
ob_data_logit$high_risk <- factor(ob_data_logit$high_risk, levels = c(0, 1))
ob_data_logit$high_risk <- factor(if_else(ob_data_logit$adm_to_del_tm > 48, 1, 0), levels = c(0, 1))
# Fit logistic regression model
logit_model <- glm(high_risk ~ age + grav + para + gest_age_days + weight + presentation_cnsldt + lbr_type_cnsldt +diff_grav_para + conditions_cnsldt,
                   family = binomial, data = ob_data_logit)

# Summary of the model
summary(logit_model)

# Tidy the model summary for clean output
tidy_logit <- tidy(logit_model)
print(tidy_logit)

library(ggplot2)

# Plotting the odds ratios and confidence intervals
# Plotting the odds ratios and confidence intervals
ggplot(tidy_logit, aes(x = term, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
    coord_flip() +
    labs(x = "Predictors", y = "Odds Ratio", title = "Odds Ratios and 95% Confidence Intervals") +
    theme_minimal() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    scale_y_continuous(trans = 'log10', breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    annotate("text", x = seq_along(tidy_logit$term), y = tidy_logit$estimate, 
             label = paste("p-value =", round(tidy_logit$p.value, 4)), hjust = 0, vjust = -0.5, size = 3)



# #Predictive modelling:----
# # Assuming 'ob_data_mdl' is loaded and ready, and 'event' is a binary outcome variable
# library(caret)
# set.seed(123)  # For reproducibility
# 
# table(ob_data_compact$event)
# 
# ob_data_compact$event <- factor(ob_data_compact$event, levels = c(0, 1))
# 
# # Ensure the package is loaded
# library(caret)
# 
# # Attempt partitioning again
# split <- createDataPartition(ob_data_compact$event, p = 0.75, list = FALSE)
# training_set <- ob_data_compact[split, ]
# testing_set <- ob_data_compact[-split, ]
# 
# # Training a logistic regression model
# fit <- train(event ~ ., data = training_set, method = "glm", family = "binomial")
# 
# # Making predictions and evaluating the model
# predictions <- predict(fit, newdata = testing_set)
# confusionMatrix(predictions, testing_set$event)
# 
# ##############################
# ##############################
# 
##ML -- multinom----
library(nnet)  # for multinom function


# Convert categorical variables to factor if not already
# ob_data_compact$membrane_rupture <- as.factor(ob_data_compact$membrane_rupture)
# ob_data_compact$vbac <- as.factor(ob_data_compact$vbac)
# ob_data_compact$delivery_method <- as.factor(ob_data_compact$delivery_method)

#1 Intrapartal Events.----

# Fit the multinomial logistic regression model
multinom_model <- multinom(intrapartal_events ~ age + grav + para + gest_age_days + weight + presentation_cnsldt + lbr_type_cnsldt +diff_grav_para + conditions_cnsldt, data = ob_data_compact)

# Summary of the model
summary(multinom_model)

# Coefficients of the model
coef(summary(multinom_model))


library(ggplot2)
library(broom)

# Obtain a tidy dataframe of the model's coefficients
tidy_model <- tidy(multinom_model)

library(ggplot2)

# Coefficient plot with the corrected faceting variable
ggplot(tidy_model, aes(x = term, y = estimate, fill = p.value < 0.05)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~ y.level, scales = "free") +
    labs(title = "Coefficient plot of Multinomial Logistic Regression",
         x = "Predictors",
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2 Intrapartal conditions:----

# Assuming the nnet package is still loaded, we'll fit the model to the 'conditions_cnsldt' variable
multinom_model_conditions <- multinom(conditions_cnsldt ~ age + grav + para + gest_age_days + weight + presentation_cnsldt + lbr_type_cnsldt +diff_grav_para + conditions_cnsldt, data = ob_data_compact)

# Summary of the model
summary(multinom_model_conditions)

# Obtain a tidy dataframe of the model's coefficients for plotting
tidy_model_conditions <- tidy(multinom_model_conditions)

# Coefficient plot for 'conditions_cnsldt'
ggplot(tidy_model_conditions, aes(x = term, y = estimate, fill = p.value < 0.05)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~ y.level, scales = "free") +
    labs(title = "Coefficient plot for Conditions Consolidated Multinomial Logistic Regression",
         x = "Predictors",
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Cluster Analysis----

setwd(getwd())
mdl_clustering <- ob_data_compact %>% 
    dplyr::select(age, gest_age_days, weight)

mdl_clustering_scaled <- scale(mdl_clustering)


# Cluster Analysis
set.seed(123)

# Perform K-means clustering
k <- 3
kmeans_result <- kmeans(mdl_clustering_scaled, centers = k, nstart = 25)

# Add the cluster assignments to the original dataset
ob_data_compact$cluster <- as.factor(kmeans_result$cluster)

# Map numeric cluster IDs to meaningful names
cluster_names <- c("Full term", "Preterm", "Early Term")
names(cluster_names) <- 1:3
ob_data_compact$cluster <- factor(ob_data_compact$cluster, levels = 1:3, labels = cluster_names)

# Now, summarizing clusters
cluster_summary <- ob_data_compact %>%
    group_by(cluster) %>%
    summarise(across(c(age, gest_age_days, weight), mean, na.rm = TRUE))

print(cluster_summary)

# Plotting
library(ggplot2)

ggplot(ob_data_compact, aes(x = gest_age_days, y = weight, color = cluster)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = c(3000, 4000), linetype = "dashed", color = "red") +
    geom_vline(xintercept = 259, linetype = "dashed", color = "blue") +
    theme_minimal() +
    scale_x_continuous(
        name = "Gestational Age in completed Weeks",  # Rename the x-axis to "Weeks"
        breaks = seq(0, max(ob_data_compact$gest_age_days), by = 7),  # Set breaks every 7 days
        labels = function(x) floor(x / 7)  # Convert days to floor weeks
    ) +
    labs(title = "Cluster Analysis on Gestational Age and Birth Weights",
         x = "Gestational Age", y = "Delivery Birthweight")


#ANOVA test on Clusters: ----

# Ensure 'cluster' is a factor with meaningful levels
ob_data_compact$cluster <- factor(ob_data_compact$cluster)

# Rename the 'cluster' variable for ANOVA
ob_data_compact$cluster_anova <- ob_data_compact$cluster

anova_result <- aov(weight ~ gest_age_days + cluster_anova, data = ob_data_compact)

# Extract the p-value for the cluster effect from the ANOVA result
p_value_cluster <- summary(anova_result)[[1]]$`Pr(>F)`[2]

# Print the p-value for the cluster effect
print(p_value_cluster)


# Load necessary library
library(MASS)

# Fit MANOVA model
manova_result <- manova(cbind(gest_age_days, weight) ~ cluster, data = ob_data_compact)
summary(manova_result)

# Detailed summary using Pillai's trace, Wilks' lambda, Hotelling-Lawley trace, or Roy's largest root
summary(manova_result, test = "Pillai")


# Assuming 'duration_of_labor' is your outcome variable
anova_result <- aov(adm_to_del_tm ~ cluster, data = ob_data_compact)
summary(anova_result)

# Extract the p-value for the ANOVA test
anova_table <- summary(anova_result)[[1]]  # This retrieves the table part of the summary
p_value <- anova_table["cluster", "Pr(>F)"]  # Correctly access the p-value for the cluster row

# Check if the ANOVA p-value is less than 0.05
if (!is.na(p_value) && p_value < 0.05) {
    tukey_test <- TukeyHSD(anova_result)
    print(tukey_test)
} else {
    cat("The ANOVA result is not significant, so no post-hoc test is performed.\n")
}

# # Assuming 'duration_of_labor', 'apgar_score', and 'other_outcome' are your outcome variables
# manova_result <- manova(cbind(adm_to_del_tm, grav, para, diff_grav_para) ~ cluster, data = ob_data_compact)
# summary(manova_result, test = "Pillai")  # You can also use Wilks, Hotelling-Lawley, or Roy's test
# 
# 
# if (summary(manova_result, test = "Pillai")$stats["Pillai", "Pr(>F)"] < 0.05) {
#     print("Significant MANOVA result, proceeding with univariate ANOVAs:")
#     aov_duration <- aov(adm_to_del_tm ~ cluster, data = ob_data_compact)
#     summary(adm_to_del_tm)
#     aov_apgar <- aov(grav ~ cluster, data = ob_data_compact)
#     summary(aov_grav)
#     aov_apgar <- aov(para ~ cluster, data = ob_data_compact)
#     summary(aov_para)
#     aov_other <- aov(diff_grav_para ~ cluster, data = ob_data_compact)
#     summary(aov_diff_grav_para)
# } else {
#     cat("MANOVA result not significant. No further univariate ANOVAs needed.\n")
# }
# 

if (p_value < 0.05) {
    print("Significant MANOVA result, proceeding with univariate ANOVAs:")
    aov_adm_to_del_tm <- aov(adm_to_del_tm ~ cluster, data = ob_data_compact)
    summary(aov_adm_to_del_tm)
    aov_grav <- aov(grav ~ cluster, data = ob_data_compact)
    summary(aov_grav)
    aov_para <- aov(para ~ cluster, data = ob_data_compact)
    summary(aov_para)
    aov_diff_grav_para <- aov(diff_grav_para ~ cluster, data = ob_data_compact)
    summary(aov_diff_grav_para)
} else {
    cat("MANOVA result not significant. No further univariate ANOVAs needed.\n")
}

#Plot ANOVA----

# Function to create and print box plots for each outcome variable
create_box_plot <- function(data, var_name) {
    p <- ggplot(data, aes(x = cluster, y = get(var_name), fill = cluster)) +
        geom_boxplot() +
        labs(title = paste("Box Plot of", var_name, "by Cluster"),
             x = "Cluster",
             y = var_name) +
        theme_minimal() +
        theme(legend.position = "none")  # Remove the legend if not needed
    
    print(p)
}

# List of outcome variables
outcome_vars <- c("adm_to_del_tm", "grav", "para", "diff_grav_para")

# Generate box plots for each variable
for(var in outcome_vars) {
    create_box_plot(ob_data_compact, var)
}

#Cluster as dependent multinom----

# Fit the multinomial logistic regression model
multinom_model <- multinom(cluster ~ age + grav + para + diff_grav_para, data = ob_data_compact)

# Tidy the model output
tidy_model <- tidy(multinom_model, conf.int = TRUE, exponentiate = FALSE)

# Check the first few rows to ensure the output is correct
print(head(tidy_model))

# Coefficient plot for multinomial logistic regression with p-value indication
ggplot(tidy_model, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange(aes(color = p.value < 0.05)) +  # Color change based on significance
    scale_color_manual(values = c("red", "blue"), labels = c("Significant", "Not Significant"), 
                       name = "Significance (p < 0.05)") +
    facet_wrap(~y.level, scales = "free") +  # Facet by cluster levels
    labs(title = "Effects of Predictors on Cluster Membership",
         x = "Predictors",
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text angle for readability


#########


# #Cluster + Cox Survival Analysis:----
# 
# # Ensure 'event' is a binary factor (if it's not already)
# ob_data_compact$event <- factor(ob_data_compact$event, levels = c(0, 1))
# 
# # Assign meaningful names to clusters
# cluster_names <- c("Fullterm", "Preterm", "Early Term")
# names(cluster_names) <- 1:3
# ob_data_compact$cluster <- factor(ob_data_compact$cluster, levels = 1:3, labels = cluster_names)
# 
# # Prepare the survival object
# #surv_obj <- with(ob_data_compact, Surv(time = adm_to_del_tm, event = event))
# # Prepare the survival object
# surv_obj <- with(ob_data_compact, Surv(time = adm_to_del_tm, event = event))
# 
# # Ensure 'cluster' is a factor
# ob_data_compact$cluster <- as.factor(ob_data_compact$cluster)
# 
# # Create survfit object directly with the formula and data
# surv_fit_cluster <- survfit(Surv(adm_to_del_tm, event) ~ cluster, data = ob_data_compact)
# 
# # Plot the survival curves for each cluster
# ggsurvplot(surv_fit_cluster, data = ob_data_compact, pval = TRUE, conf.int = TRUE,
#            palette = "Dark2",
#            xlab = "Time to Delivery (Hours)", ylab = "Survival Probability",
#            title = "Survival Curves by Cluster")
# ggforest3(cox_model, data = ob_data_compact)


#Coc cluster----

# Load necessary library
library(survival)
ob_data_compact <- readRDS("OB Data with consolidated Intrapartal conditions, labor type and presentation.RDS")

mdl_clustering <- ob_data_compact %>% 
    dplyr::select(age, gest_age_days, weight)

mdl_clustering_scaled <- scale(mdl_clustering)


# Cluster Analysis
set.seed(123)

# Perform K-means clustering
k <- 3
kmeans_result <- kmeans(mdl_clustering_scaled, centers = k, nstart = 25)

# Add the cluster assignments to the original dataset
ob_data_compact$cluster <- as.factor(kmeans_result$cluster)

# Map numeric cluster IDs to meaningful names
cluster_names <- c("Full term", "Preterm", "Early Term")
names(cluster_names) <- 1:3
ob_data_compact$cluster <- factor(ob_data_compact$cluster, levels = 1:3, labels = cluster_names)


ob_data_compact$cluster <- factor(ob_data_compact$cluster)
# Rename the 'cluster' variable for Cox
ob_data_compact$cluster_cox <- ob_data_compact$cluster

# Fit Cox proportional hazards model
cox_cluster_model <- coxph(Surv(adm_to_del_tm, event) ~ cluster_cox + weight + gest_age_days, data = ob_data_compact)

# Summary of the Cox model
summary(cox_cluster_model)

# fit <- coxph(Surv(time, status) ~
#                  sex + ph.ecog + age +
#                  strata(sex),
#              data = lung)
# ggcoxadjustedcurves(fit, data=lung)

# Fit the Cox model
fit_cluster_cox <- coxph(Surv(adm_to_del_tm, event) ~ cluster_cox + weight + gest_age_days + strata(cluster_cox), data = ob_data_compact)

# Generate adjusted survival curves using the fitted model
# Explicitly pass the data frame used in the model fitting
ggadjustedcurves(fit = fit_cluster_cox, data = ob_data_compact)
#ggadjustedcurves(fit = fit_cluster_cox, variable = "cluster_cox", data = ob_data_compact, fun = "cumhaz")

#cumhaz; cum-event
surv_fit <- survfit(Surv(adm_to_del_tm, event) ~ cluster_cox, data = ob_data_compact)
# Plotting the Survival Probability
plot(surv_fit, fun = "event", col = 1:3, main = "Survival Probability",
     xlab = "Time", ylab = "Delivery-as-Event Probability", mark.time = TRUE)
legend("bottomright", legend = levels(ob_data_compact$cluster_cox), col = 1:3, lty = 1)

# Plotting the Cumulative Hazard
plot(surv_fit, fun = "cumhaz", col = 1:3, main = "Cumulative Hazard",
     xlab = "Time", ylab = "Cumulative Hazard of Delivery_as_Event", mark.time = TRUE)
legend("topleft", legend = levels(ob_data_compact$cluster_cox), col = 1:3, lty = 1)


# Compute Kaplan-Meier survival estimate
surv_fit_cluster <- survfit(cox_cluster_model)
ggsurvplot(surv_fit_cluster, data = ob_data_compact, conf.int = FALSE, risk.table = TRUE)

surv_fit_cluster <- survfit(cox_cluster_model)
ggsurvplot(surv_fit_cluster, data = ob_data_compact, conf.int = FALSE, risk.table = TRUE, fun = "event")

ggforest3(cox_cluster_model, data = ob_data_compact)

##




#Multinom Cluster----

#1 Intrapartal Conditions.----

# Fit the multinomial logistic regression model
multinom_model_cluster <- multinom(conditions_cnsldt ~ cluster, data = ob_data_compact)

# Summary of the model
summary(multinom_model_cluster)

# Coefficients of the model
coef(summary(multinom_model_cluster))


library(ggplot2)
library(broom)

# Obtain a tidy dataframe of the model's coefficients
tidy_model_cluster <- tidy(multinom_model_cluster)

library(ggplot2)

# Coefficient plot with the corrected faceting variable
ggplot(tidy_model_cluster, aes(x = term, y = estimate, fill = p.value < 0.05)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~ y.level, scales = "free") +
    labs(title = "Coefficient plot of Multinomial Logistic Regression",
         x = "Predictors",
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ZipCode, Map from UDS data ----
#From Analysis project and saved RDS File

ob_data_uds <- readRDS("UDS OB DATA for Modelling on Birth Weight.RDS")
summary(ob_data_uds)
ob_data_zip <- ob_data_uds %>% 
    dplyr::select(city, zip, age, uds_age, gest_age_days=gestational_days, weight = delivery_birthweight_num2)

#UDS data cluster

mdl_clustering_uds <- ob_data_zip %>% 
    dplyr::select(age, gest_age_days, weight)

mdl_clustering_uds_scaled <- scale(mdl_clustering_uds)


# Cluster Analysis
set.seed(333)

# Perform K-means clustering
k <- 3
kmeans_result_uds <- kmeans(mdl_clustering_uds_scaled, centers = k, nstart = 25)

# Add the cluster assignments to the original dataset
ob_data_zip$cluster <- as.factor(kmeans_result_uds$cluster)

# Map numeric cluster IDs to meaningful names
cluster_names <- c("Full term", "Preterm", "Early Term")
names(cluster_names) <- 1:3
ob_data_zip$cluster <- factor(ob_data_zip$cluster, levels = 1:3, labels = cluster_names)

# Now, summarizing clusters
cluster_summary_zip <- ob_data_zip %>%
    group_by(cluster) %>%
    summarise(across(c(age, gest_age_days, weight), mean, na.rm = TRUE))

print(cluster_summary_zip)

# Plotting
library(ggplot2)

ggplot(ob_data_zip, aes(x = gest_age_days, y = weight, color = cluster)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = c(3000, 4000), linetype = "dashed", color = "red") +
    geom_vline(xintercept = 259, linetype = "dashed", color = "blue") +
    theme_minimal() +
    scale_x_continuous(
        name = "Gestational Age in completed Weeks",  # Rename the x-axis to "Weeks"
        breaks = seq(0, max(ob_data_compact$gest_age_days), by = 7),  # Set breaks every 7 days
        labels = function(x) floor(x / 7)  # Convert days to floor weeks
    ) +
    labs(title = "Cluster Analysis on Gestational Age and Birth Weights",
         x = "Gestational Age", y = "Delivery Birthweight")

#ANOVA test on Clusters: ----

# Ensure 'cluster' is a factor with meaningful levels
ob_data_zip$cluster <- factor(ob_data_zip$cluster)

# Rename the 'cluster' variable for ANOVA
ob_data_zip$cluster_anova <- ob_data_zip$cluster

anova_result_zip <- aov(weight ~ gest_age_days + cluster_anova, data = ob_data_zip)

# Extract the p-value for the cluster effect from the ANOVA result
p_value_cluster_zip <- summary(anova_result_zip)[[1]]$`Pr(>F)`[2]

# Print the p-value for the cluster effect
print(p_value_cluster_zip)

# Load necessary library
library(MASS)

# Fit MANOVA model
manova_result_zip <- manova(cbind(gest_age_days, weight) ~ cluster, data = ob_data_zip)
summary(manova_result_zip)

# Detailed summary using Pillai's trace, Wilks' lambda, Hotelling-Lawley trace, or Roy's largest root
summary(manova_result_zip, test = "Pillai")


#

# Extract the p-value for the ANOVA test
anova_table <- summary(anova_result_zip)[[1]]  # This retrieves the table part of the summary
p_value <- anova_table["cluster", "Pr(>F)"]  # Correctly access the p-value for the cluster row

# # Check if the ANOVA p-value is less than 0.05
# if (!is.na(p_value) && p_value < 0.05) {
#     tukey_test <- TukeyHSD(anova_result_zip)
#     print(tukey_test)
# } else {
#     cat("The ANOVA result is not significant, so no post-hoc test is performed.\n")
# }

# Zip Multinomial logistic regression model ----
ob_data_zip$zip <- factor(ob_data_zip$zip, levels = levels(factor(ob_data_zip$zip)))
multinom_model_cluster_zip <- multinom(cluster ~ zip, data = ob_data_zip)

# Summary of the model
summary(multinom_model_cluster_zip)

# Coefficients of the model
coef(summary(multinom_model_cluster_zip))


library(ggplot2)
library(broom)

# Obtain a tidy dataframe of the model's coefficients
tidy_model_cluster_zip <- tidy(multinom_model_cluster_zip)

library(ggplot2)

# Coefficient plot with the corrected faceting variable
ggplot(tidy_model_cluster_zip, aes(x = term, y = estimate, fill = p.value < 0.05)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~ y.level, scales = "free") +
    labs(title = "Coefficient plot of Multinomial Logistic Regression",
         x = "Predictors",
         y = "Coefficient Estimate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


