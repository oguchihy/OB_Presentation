#setwd("C:/Users/onkwocha/Dropbox/CSVS/OB Service/NMC OB LOG")

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


# Assuming your data frame is named ob_data_ans and you've selected some numeric columns
# Standardizing the data
ob_data_reg <- readRDS("OB DATA no NAs.RDS")
#install.packages("mice")

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

#Random Forest

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

#Survival Analysis with surminer

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


# Fit the Cox model
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

# Now tidy your Cox model
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

# # Plot using ggplot2 to check the setup
# library(ggplot2)
# ggplot(tidy_cox, aes(x = reorder(term, estimate), y = estimate)) +
#     geom_point() +
#     geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
#     coord_flip() +
#     labs(x = "Term", y = "Hazard Ratio (HR)") +
#     theme_minimal()

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

#Cox
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

# Extract and prepare model output for visualization
# library(broom)
# tidy_cox <- tidy(cox_model, exponentiate = TRUE)  # Convert coefficients to HR
# 
# # Visualize HR with forest plot using ggforestplot if available or survminer's ggforest
# if("ggforestplot" %in% rownames(installed.packages())) {
#     library(ggforestplot)
#     forestplot(tidy_cox,
#                  estimate_col = "estimate",
#                  conf_low_col = "conf.low",
#                  conf_high_col = "conf.high",
#                  label_col = "term",
#                  pvalue_col = "p.value",
#                  is.log.scale = TRUE)
# } else {
#     library(survminer)
#     ggforest(cox_model)
# }



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

# Fit the Cox proportional hazards model
cox_model <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days + weight + diff_grav_para + intrapartal_conditions + intrapartal_events,
                   data = ob_data_mdl)

# Use survminer's ggforest to visualize the model's output
ggforest(cox_model, data = ob_data_mdl)


##Consolidate intrapartalconditions and events

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


##Consolidate labor_type and presentation

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

#Rerun Cox model ----

library(survival)
library(survminer)

# Load the dataset
ob_data_compact <- readRDS("OB Data with consolidated Intrapartal conditions, labor type and presentation.RDS")

# Check and prepare variables
if(!"event" %in% names(ob_data_compact) || !is.numeric(ob_data_compact$adm_to_del_tm)) {
    ob_data_compact$adm_to_del_tm <- as.numeric(ob_data_compact$adm_to_del_tm)
    ob_data_mdl$event <- ifelse(!is.na(ob_data_mdl$delivery_date), 1, 0)  # Assuming event is defined like this
}
#!. Intrapartal EVENTS

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



# Intrapartal conditions
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
setwd(getwd())
install.packages("C:/Users/onkwocha/Dropbox", "myRFunctions", type = "source")
library(ggforest3)
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
surv_fit <- survfit(fit)

# Plot cumulative hazard using survminer
ggsurvplot(surv_fit, data = ob_data_compact, fun = "cumhaz")

ggsurvplot(fit, data=ob_data_compact, fun = "cumhaz")


library(survival)
library(survminer)

# Fit Cox model
fit <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days + weight + diff_grav_para + conditions_cnsldt,
             data = ob_data_compact)

# Compute Kaplan-Meier survival estimate
surv_fit <- survfit(fit)

# Plot cumulative survival over time
ggsurvplot(surv_fit, data = ob_data_compact, conf.int = FALSE, risk.table = TRUE, fun = "event")





#Regression
library(stats)
library(broom)

# Ensure the dataset is loaded
# ob_data_mdl <- readRDS("Cleaned OB Dataset Ready for Analysis.RDS")
# 
# # If 'high_risk' is not already in your dataset, create it for demonstration purposes
# # This is a placeholder: you should replace it with your actual method of defining high risk
# if(!"high_risk" %in% names(ob_data_mdl)) {
#     ob_data_mdl$high_risk <- ifelse(ob_data_mdl$adm_to_del_tm > 48, 1, 0)
# }
# 
# # Convert high_risk to a factor for logistic regression
# ob_data_mdl$high_risk <- factor(ob_data_mdl$high_risk, levels = c(0, 1))
ob_data_mdl$high_risk <- factor(if_else(ob_data_mdl$adm_to_del_tm > 48, 1, 0), levels = c(0, 1))
# Fit logistic regression model
logit_model <- glm(high_risk ~ age + grav + para + gest_age_days + intrapartal_conditions + intrapartal_events,
                   family = binomial, data = ob_data_mdl)

# Summary of the model
summary(logit_model)

# Tidy the model summary for clean output
tidy_logit <- tidy(logit_model)
print(tidy_logit)

library(ggplot2)
library(broom)

# Assuming 'logit_model' is already fitted
#tidy_logit <- tidy(logit_model, exponentiate = TRUE)  # Exponentiate coefficients to get Odds Ratios
# Assuming 'logit_model' is your model
tidy_logit <- tidy(logit_model, exponentiate = TRUE, conf.int = TRUE)

library(ggplot2)

ggplot(tidy_logit, aes(x = term, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    coord_flip() +  # Makes the term labels horizontal
    labs(x = "Predictors", y = "Odds Ratio", title = "Odds Ratios and 95% Confidence Intervals") +
    theme_minimal() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Line at Odds Ratio = 1 for reference
    scale_y_continuous(trans = 'log10', breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x)))

#Predictive modelling:
# Assuming 'ob_data_mdl' is loaded and ready, and 'event' is a binary outcome variable
library(caret)
set.seed(123)  # For reproducibility

table(ob_data_mdl$event)

ob_data_mdl$event <- factor(ob_data_mdl$event, levels = c(0, 1))

# Ensure the package is loaded
library(caret)

# Attempt partitioning again
split <- createDataPartition(ob_data_mdl$event, p = 0.75, list = FALSE)
training_set <- ob_data_mdl[split, ]
testing_set <- ob_data_mdl[-split, ]

# Training a logistic regression model
fit <- train(event ~ ., data = training_set, method = "glm", family = "binomial")

# Making predictions and evaluating the model
predictions <- predict(fit, newdata = testing_set)
confusionMatrix(predictions, testing_set$event)


##ML
library(nnet)  # for multinom function



#########################################################
##########################################################

library(nnet)  # for multinom function

# Load necessary library
library(nnet)

# Convert categorical variables to factor if not already
ob_data_compact$membrane_rupture <- as.factor(ob_data_compact$membrane_rupture)
ob_data_compact$vbac <- as.factor(ob_data_compact$vbac)
ob_data_compact$delivery_method <- as.factor(ob_data_compact$delivery_method)

#1 Intrapartal Events.

# Fit the multinomial logistic regression model
multinom_model <- multinom(intrapartal_events ~ age + grav + para + adm_to_del_tm + diff_grav_para + membrane_rupture + vbac + delivery_method, data = ob_data_compact)

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

#2 Intrapartal conditions:

# Assuming the nnet package is still loaded, we'll fit the model to the 'conditions_cnsldt' variable
multinom_model_conditions <- multinom(conditions_cnsldt ~ age + grav + para + adm_to_del_tm + diff_grav_para + membrane_rupture + vbac + delivery_method, data = ob_data_compact)

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



#Cluster Analysis

setwd(getwd())
mdl_clustering <- ob_data_compact %>% 
    dplyr::select(age, gest_age_days, weight)

mdl_clustering_scaled <- scale(mdl_clustering)

# Perform K-means clustering
set.seed(123)
k <- 3
kmeans_result <- kmeans(mdl_clustering_scaled, centers = k, nstart = 25)

# Add the cluster assignments to the original dataset
mdl_clustering$cluster <- as.factor(kmeans_result$cluster)

# Now, summarizing clusters
cluster_summary <- mdl_clustering %>%
    group_by(cluster) %>%
    summarise(across(c(age, gest_age_days, weight), mean, na.rm = TRUE))

print(cluster_summary)

ggplot(mdl_clustering, aes(x = gest_age_days, y = weight, color = cluster)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = c(3000, 4000), linetype = "dashed", color = "red") +
    geom_vline(xintercept = 259, linetype = "dashed", color = "blue") +
    theme_minimal() +
    scale_x_continuous(
        name = "Gestational Age in completed Weeks",  # Rename the x-axis to "Weeks"
        breaks = seq(0, max(mdl_clustering$gest_age_days), by = 7),  # Set breaks every 7 days
        labels = function(x) floor(x / 7)  # Convert days to floor weeks
    ) +
    labs(title = "Cluster Analysis on Gestational Age and Birth Weights",
         x = "Gestational Age", y = "Delivery Birthweight")
