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
library(ggforrest)

ob.data.db <- readRDS("Base OB Data DB.RDS") 

#Add features calc vars

ob.data.augm <- ob.data.db %>% 
    mutate(
        # calculate time from admission to Del in hours
        adm_to_del_tm = as.double(difftime(delivery_datetime, adm_datetime, units = "hours")),
        adm_to_del_tm = if_else(adm_to_del_tm <=0, .001, adm_to_del_tm),
        adm_to_del_tm = round(adm_to_del_tm, 0),
        adm_to_del_tm = if_else(adm_to_del_tm==0, 12+ adm_to_del_tm, adm_to_del_tm),
        
        #Calculate Gravida-Parity difference
        diff_grav_para = grav-para,
        diff_grav_para = if_else(diff_grav_para <=0, 1, diff_grav_para)
        ) 
saveRDS(ob.data.augm, "Full DB OB DATA.RDS")
# Summary Statistics
#summary(ob.data.augm)

ob_data_eda <- readRDS("Full DB OB DATA.RDS")
#explore(ob_data_eda)

ob_data_ans <- readRDS("Full DB OB DATA.RDS") %>% 
    filter(adm_to_del_tm < 300) %>% 
    select(-viability, monitor, baby_sq)
ob_data_ans$vbac <- ifelse(is.na(ob_data_ans$vbac) | ob_data_ans$vbac == "N/A", "None", ob_data_ans$vbac)
ob_data_ans$operative_vaginal<- ifelse(is.na(ob_data_ans$operative_vaginal) | ob_data_ans$operative_vaginal == "N/A", "None", ob_data_ans$operative_vaginal)
ob_data_ans$membrane_rupture<- ifelse(is.na(ob_data_ans$membrane_rupture) | ob_data_ans$membrane_rupture == "N/A", "None", ob_data_ans$membrane_rupture)
ob_data_ans$intrapartal_events<- ifelse(is.na(ob_data_ans$intrapartal_events) | ob_data_ans$intrapartal_events == "N/A", "None", ob_data_ans$intrapartal_events)
ob_data_ans$intrapartal_conditions<- ifelse(is.na(ob_data_ans$intrapartal_conditions) | ob_data_ans$intrapartal_conditions == "N/A", "None", ob_data_ans$intrapartal_conditions)

# Install if not already installed
# install.packages("survival")

library(survival)

# Assuming all entries in 'adm_to_del_tm' are events (no censoring)
surv_object <- Surv(time = ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans)))

# Fit Kaplan-Meier survival curve
km_fit <- survfit(surv_object ~ 1)  # '~ 1' indicates no stratification; use covariates for stratification

# Plot the survival curve
plot(km_fit, xlab = "Hours from Admission to Delivery", ylab = "Probability of Delivery",
     main = "Kaplan-Meier Survival Curve", col = "blue")




# Assuming you've already loaded the 'survival' library and have ob_data_ans ready.

# Make sure the dataset is properly prepared with no NA values in the covariates
ob_data_no_na <- na.omit(ob_data_ans)

saveRDS(ob_data_no_na, "OB DATA no NAs.RDS")
ob_data_km <- readRDS("OB DATA no NAs.RDS")

# Fit Kaplan-Meier survival curve for ob_data_ans
surv_object <- Surv(time = ob_data_km$adm_to_del_tm, event = rep(1, nrow(ob_data_km)))
km_fit <- survfit(surv_object ~ 1)

# Plot the Kaplan-Meier survival curve
plot(km_fit, xlab = "Hours from Admission to Delivery", ylab = "Probability of Delivery",
     main = "Kaplan-Meier Survival Curve", col = "blue")


# Fit Cox proportional hazards model on the same dataset (ob_data_km)
cox_model <- coxph(Surv(adm_to_del_tm, event = rep(1, nrow(ob_data_km))) ~ age + grav + para + diff_grav_para + intrapartal_conditions + intrapartal_events, 
                       data = ob_data_km)

# Print the summary of the Cox model
summary(cox_model)


##Hazard Ratio

# Calculate Hazard Ratios
hr <- exp(coef(cox_model))

# Calculate confidence intervals for the hazard ratios
hr_confint <- confint(cox_model)
hr_confint_exp <- exp(hr_confint)

# Assuming 'cox_model' is your Cox model object
summary_cox <- summary(cox_model)  # Get the summary of the Cox model

# Create a data frame for the hazard ratios, confidence intervals, and p-values
hr_table <- data.frame(
    Variable = rownames(summary_cox$coefficients),
    Hazard_Ratio = exp(summary_cox$coefficients[, "coef"]),  # Convert log HR to HR
    CI_Lower = exp(summary_cox$conf.int[, 1]),  # Convert log CI to regular CI
    CI_Upper = exp(summary_cox$conf.int[, 2]),  # Convert log CI to regular CI
    P_Value = summary_cox$coefficients[, "Pr(>|z|)"]
)
# hr_table <- data.frame(
#     Variable = rownames(summary_cox$coefficients),
#     Hazard_Ratio = summary_cox$coefficients[, "coef"],  # Convert log HR to HR
#     CI_Lower = summary_cox$conf.int[, 1],  # Convert log CI to regular CI
#     CI_Upper = summary_cox$conf.int[, 2],  # Convert log CI to regular CI
#     P_Value = summary_cox$coefficients[, "Pr(>|z|)"]
#)





# Let's print the table to see if it's correctly formatted
print(hr_table)


# Order by Hazard Ratio
hr_table_srt <- hr_table[order(-hr_table$Hazard_Ratio), ]

print(hr_table)

# If not already installed
# install.packages("broom")

library(broom)

# Tidy the Cox model summary into a data frame
tidy_cox <- tidy(cox_model)

# View the tidy summary of the Cox model
print(tidy_cox)

# install.packages("devtools")
#devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)
forestplot(
     df= tidy_cox,
     name = term,
     estimate_col = "estimate",
     label_col = "term",
     pvalue = p.value,
     psignif = 0.05,
     se = std.error,
     is.log.scale = FALSE,
     plot.ci = TRUE,
     xlab = "Hazard Ratio",
     logodds = TRUE,
     plot.theme = ggplot2::theme_minimal()
)+ 
    geom_text(aes(label = sprintf("HR=%.2f, p=%.3f", exp(adjusted_estimate), p.value)), hjust = -1.2, vjust = 0.5)


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



# Install gtsummary if not already installed
if(!require(gtsummary)) install.packages("gtsummary", dependencies = TRUE)
library(gtsummary)

# Create a table with the Cox model using gtsummary
table_cox <- tbl_regression(cox_model, exponentiate = T)
print(table_cox)


library(gtsummary)
library(ggplot2)

# Assuming 'cox_model' is your Cox model
tbl_cox <- tbl_regression(cox_model, exponentiate = TRUE)

# Convert the gtsummary table to a data frame for plotting

# Filtering to include only necessary rows
plot_data <- tbl_cox$table_body %>%
    filter(row_type == "level" | (row_type == "label" & !is.na(estimate))) %>%
    select(label, estimate, conf.low, conf.high, p.value) %>%
    na.omit()  # Ensure that no NA values in necessary columns are included

# Print the filtered data to check
print(plot_data)

# Plotting using ggplot2
library(ggplot2)
ggplot(plot_data, aes(x = reorder(label, -estimate), y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    coord_flip() +  # Flipping coordinates for horizontal layout
    theme_minimal() +
    labs(x = "Variable", y = "Hazard Ratio",
         title = "Hazard Ratios with 95% Confidence Intervals for Significant Variables") +
    geom_text(aes(label = sprintf("HR=%.2f, p=%.3f", estimate, p.value), y = conf.high + 0.1), hjust = -0.2) +
    theme(axis.text.y = element_text(hjust = 1))


# ###################
# 
# 
# # # Ordering the table by Hazard Ratio
# # hr_table <- data.frame(
# #     Variable = names(hr),
# #     Hazard_Ratio = exp(hr),  # Converting log HR to HR
# #     CI_Lower = exp(hr_confint[,1]),
# #     CI_Upper = exp(hr_confint[,2])
# # )
# 
# # Now reorder the table by Hazard_Ratio in descending order
# hr_table_srt <- hr_table[order(-hr_table$Hazard_Ratio), ]
# 
# # Plotting using ggplot2
# library(ggplot2)
# ggplot(hr_table_srt, aes(x = reorder(Variable, -Hazard_Ratio), y = Hazard_Ratio)) +
#     geom_point() +
#     geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
#     coord_flip() +  # Flipping coordinates for horizontal layout
#     theme_minimal() +
#     labs(x = "Variable", y = "Hazard Ratio",
#          title = "Hazard Ratios with 95% Confidence Intervals for Significant Variables") +
#     geom_text(aes(label = sprintf("HR=%.2f", Hazard_Ratio), y = CI_Upper + 0.1), hjust = -0.2) +
#     theme(axis.text.y = element_text(angle = 45, hjust = 1))
# 
# ggplot(hr_table_srt, aes(x = reorder(Variable, -Hazard_Ratio), y = Hazard_Ratio)) +
#     geom_point() +
#     geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
#     coord_flip() +  # Flipping coordinates for horizontal layout
#     theme_minimal() +
#     labs(x = "Variable", y = "Hazard Ratio",
#          title = "Hazard Ratios with 95% Confidence Intervals for Significant Variables") +
#     geom_text(aes(label = sprintf("HR=%.2f, p=%.3f", Hazard_Ratio, P_Value), y = CI_Upper + 0.1), hjust = -0.2) +
#     theme(axis.text.y = element_text(hjust = 1))  # Removing the angle adjustment
# 
# 
# 



