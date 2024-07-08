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

ob.data.db <- readRDS("Base OB Data DB.RDS") 

#Add features calc vars

ob.data.augm <- ob.data.db %>% 
    mutate(
        # calculate time from admission to Del in hours
        adm_to_del_tm = as.double(difftime(delivery_datetime, adm_datetime, units = "hours")),
        
        #Calculate Gravida-Parity difference
        diff_grav_para = grav-para,
        diff_grav_para = if_else(diff_grav_para <=0, 1, diff_grav_para)
        ) 
saveRDS(ob.data.augm, "Full DB OB DATA.RDS")
# Summary Statistics
#summary(ob.data.augm)

ob_data_eda <- readRDS("Full DB OB DATA.RDS")
explore(ob_data_eda)

ob_data_ans <- readRDS("Full DB OB DATA.RDS") #%>% 
    #filter(adm_to_del_tm < 100)
# Install if not already installed
# install.packages("survival")

library(survival)

# Assuming all entries in 'adm_to_del_tm' are events (no censoring)
surv_object <- Surv(time = ob_data_ans <- ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans)))

# Fit Kaplan-Meier survival curve
km_fit <- survfit(surv_object ~ 1)  # '~ 1' indicates no stratification; use covariates for stratification

# Plot the survival curve
plot(km_fit, xlab = "Hours from Admission to Delivery", ylab = "Probability of Delivery",
     main = "Kaplan-Meier Survival Curve", col = "blue")




# Assuming you've already loaded the 'survival' library and have ob_data_ans ready.

# Make sure the dataset is properly prepared with no NA values in the covariates
ob_data_ans <- na.omit(ob_data_ans)



# Fit Kaplan-Meier survival curve for ob_data_ans
surv_object_ans <- Surv(time = ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans)))
km_fit_ans <- survfit(surv_object_ans ~ 1)

# Plot the Kaplan-Meier survival curve
plot(km_fit_ans, xlab = "Hours from Admission to Delivery", ylab = "Probability of Delivery",
     main = "Kaplan-Meier Survival Curve", col = "blue")

# Fit Cox proportional hazards model on the same dataset (ob_data_ans)
cox_model_ans <- coxph(Surv(adm_to_del_tm, event = rep(1, nrow(ob_data_ans))) ~ age + grav + para + intrapartal_conditions + intrapartal_events, 
                       data = ob_data_ans)

# Print the summary of the Cox model
summary(cox_model_ans)


##Hazard Ratio

# Calculate Hazard Ratios
hr <- exp(coef(cox_model_ans))

# Calculate 95% Confidence Intervals for Hazard Ratios
hr_confint <- exp(confint(cox_model_ans))

# Combine HR and CI into a data frame for easier reading
hr_table <- data.frame(
    Hazard_Ratio = hr,
    CI_Lower = hr_confint[,1],
    CI_Upper = hr_confint[,2]
)

print(hr_table)

# If not already installed
# install.packages("broom")

library(broom)

# Tidy the Cox model summary into a data frame
tidy_cox <- tidy(cox_model_ans)

# View the tidy summary of the Cox model
print(tidy_cox)


# If not already installed
# install.packages("ggplot2")

library(ggplot2)

# Plotting Hazard Ratios using ggplot2
ggplot(hr_table, aes(x = rownames(hr_table), y = Hazard_Ratio)) +
    geom_point() +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    theme_minimal() +
    labs(x = "Variable", y = "Hazard Ratio",
         title = "Hazard Ratios and 95% Confidence Intervals") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



#################

# Ensure the hr_table is created with a 'Variable' column using the correct Cox model object
hr_ans <- exp(coef(cox_model_ans))
hr_confint_ans <- exp(confint(cox_model_ans))

# Create a data frame for Hazard Ratios and their Confidence Intervals
hr_table_ans <- data.frame(
    Variable = rownames(hr_confint_ans),  # Make sure to include variable names
    Hazard_Ratio = hr_ans,
    CI_Lower = hr_confint_ans[, 1],
    CI_Upper = hr_confint_ans[, 2]
)

# Check the structure of hr_table_ans to ensure 'Variable' exists
print(head(hr_table_ans))

# If ggplot2 is not loaded, load it
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Plotting Hazard Ratios using ggplot2
ggplot(hr_table_ans, aes(x = Variable, y = Hazard_Ratio)) +
    geom_point() +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    theme_minimal() +
    labs(x = "Variable", y = "Hazard Ratio",
         title = "Hazard Ratios with 95% Confidence Intervals") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Install gtsummary if not already installed
if(!require(gtsummary)) install.packages("gtsummary", dependencies = TRUE)
library(gtsummary)

# Create a table with the Cox model using gtsummary
table_cox <- tbl_regression(cox_model_ans)
print(table_cox)
