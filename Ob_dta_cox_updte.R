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
library(survminer)

ob.data.db <- readRDS("Base OB Data DB.RDS") 

#Add features calc vars

ob.data.augm <- ob.data.db %>% 
    mutate(
        # calculate time from admission to Del in hours
        adm_to_del_tm = as.double(difftime(delivery_datetime, adm_datetime, units = "hours")),
        adm_to_del_tm = round(adm_to_del_tm, 0),
        
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
    # filter(adm_to_del_tm < 300) %>% 
    select(-viability, monitor, baby_sq)
ob_data_ans$vbac <- ifelse(is.na(ob_data_ans$vbac) | ob_data_ans$vbac == "N/A", "None", ob_data_ans$vbac)
ob_data_ans$operative_vaginal<- ifelse(is.na(ob_data_ans$operative_vaginal) | ob_data_ans$operative_vaginal == "N/A", "None", ob_data_ans$operative_vaginal)
ob_data_ans$membrane_rupture<- ifelse(is.na(ob_data_ans$membrane_rupture) | ob_data_ans$membrane_rupture == "N/A", "None", ob_data_ans$membrane_rupture)
ob_data_ans$intrapartal_events<- ifelse(is.na(ob_data_ans$intrapartal_events) | ob_data_ans$intrapartal_events == "N/A", "None", ob_data_ans$intrapartal_events)
ob_data_ans$intrapartal_conditions<- ifelse(is.na(ob_data_ans$intrapartal_conditions) | ob_data_ans$intrapartal_conditions == "N/A", "None", ob_data_ans$intrapartal_conditions)
cutoff <- quantile(ob_data_ans$adm_to_del_tm, 0.99, na.rm = TRUE)  # 99th percentile as the cutoff
ob_data_ans$adm_to_del_tm <- pmin(ob_data_ans$adm_to_del_tm, cutoff)

# Calculate the median of the 'adm_to_del_tm' while ignoring NA values
median_adm_to_del_tm <- median(ob_data_ans$adm_to_del_tm, na.rm = TRUE)

# Replace NA values with the median
ob_data_ans$adm_to_del_tm[is.na(ob_data_ans$adm_to_del_tm)] <- median_adm_to_del_tm


# Install if not already installed
# install.packages("survival")

library(survival)

# Kaplan-Meier Plot
surv_obj <- Surv(time = ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans)))  # Assuming all are events
km_fit <- survfit(surv_obj ~ 1)
# Plot the survival curve
plot(km_fit, xlab = "Hours from Admission to Delivery", ylab = "Probability of Delivery",
     main = "Kaplan-Meier Survival Curve", col = "blue")

# Cox Proportional Hazards Model
cox_model <- coxph(surv_obj ~ covariates, data = ob_data_ans)
summary(cox_model)

#Multiple / Different variables
# Plotting the survival curves with different colors for each gender
# Kaplan-Meier Plot by Gender Only
km_fit_gender <- survfit(surv_obj ~ gender, data = ob_data_ans)

plot(km_fit_gender, col = 1:2, lty = 1:2, xlab = "Hours from Admission to Delivery", ylab = "Probability of Delivery",
     main = "Survival Curves by Gender")

# Adding a more detailed legend with the confirmed strata names
legend("topright",  # Position the legend in the top right corner
       legend = strata_names, 
       col = 1:2,  # Assuming two genders are plotted
       lty = 1:2,  # Line types corresponding to the gender strata
       title = "Gender",
       cex = 0.8,  # Text size
       bty = "n")  # No box around the legend

## Manually setting breaks based on the observed range and clinical relevance
breaks <- c(1, 3, 5, 7)  # Example breaks based on clinical judgment or data distribution

ob_data_ans <- transform(ob_data_ans, diff_grav_para_cat = cut(diff_grav_para,
                                                               breaks = breaks,
                                                               labels = c("Low", "Medium", "High"),
                                                               include.lowest = TRUE))


surv_obj_cat <- Surv(time = ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans)))
km_fit_cat <- survfit(surv_obj_cat ~ diff_grav_para_cat, data = ob_data_ans)

# Plot the survival curves by categories
plot(km_fit_cat, col = 1:3, xlab = "Hours from Admission to Delivery", ylab = "Probability of Delivery",
     main = "Survival Curves by diff_grav_para Categories")
legend("topright", legend = levels(ob_data_ans$diff_grav_para_cat), col = 1:3, lty = 1:3, title = "Categories")

#Continuous variables:
# Load necessary library
library(survival)

# Ensure the 'survival' package is loaded for these functions
if (!requireNamespace("survival", quietly = TRUE)) {
    install.packages("survival")
}

# Prepare the survival object
surv_obj <- Surv(time = ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans)))  # Assuming all are events

# Fit a Cox proportional hazards model including 'diff_grav_para' as a continuous variable
cox_model <- coxph(surv_obj ~ diff_grav_para, data = ob_data_ans)

# Summary of the Cox model
summary(cox_model)

# Plotting the survival curves based on the Cox model
ggsurvplot(
    cox_model, 
    data = ob_data_ans,
    fun = "cumhaz",  # Function to transform survival probabilities, 'cumhaz' for cumulative hazard
    ggtheme = theme_minimal(),  # Using a minimal theme for the plot
    risk.table = TRUE,  # Adds a risk table at the bottom
    pval = TRUE,  # Show p-value of the overall model
    conf.int = TRUE,  # Show confidence intervals
    xlab = "Time (Hours)",  # Label for the x-axis
    ylab = "Cumulative Hazard",  # Label for the y-axis
    break.time.by = 10,  # Breaks for the x-axis, adjust as needed
    surv.scale = "percent"  # Scaling the survival probabilities
)



# Load necessary package
if (!require(survminer)) {
    install.packages("survminer")
    library(survminer)
}

# Assuming cox_model is correctly specified and created
# Plotting the survival curves based on the Cox model
ggsurvplot(
    fit = fit,
    data = ob_data_ans,
    fun = "cumhaz",  # Cumulative hazard function
    ggtheme = theme_minimal(),  # Minimal theme for clarity
    risk.table = TRUE,  # Include a table of number at risk
    pval = TRUE,  # Display p-value of the Cox model
    conf.int = TRUE,  # Show confidence intervals
    xlab = "Time (Hours)",  # X-axis label
    ylab = "Cumulative Hazard",  # Y-axis label
    surv.scale = "percent",  # Display survival probability in percent
    break.time.by = 10  # Setting breaks on the x-axis
)



#survminer http://www.sthda.com/english/wiki/survminer-r-package-survival-data-analysis-and-visualization
#https://rpkgs.datanovia.com/survminer/survminer_cheatsheet.pdf

fit <- survfit(Surv(time = ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans))) ~ 1, data = ob_data_ans)
# Drawing curves
ggsurvplot(fit, color = "#2E9FDF")

fit <- survfit(Surv(time = ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans))) ~ gender, data = ob_data_ans)
ggsurvplot(fit, legend = "bottom", legend.title = "Baby gender", legend.labs = c("Male", "Female"), conf.int = TRUE,  pval = TRUE)

ggsurvplot(fit, conf.int = TRUE, palette = c("#FF9E29", "#86AA00"), risk.table = TRUE, risk.table.col = "strata", fun = "event")

ggsurvplot(fit, conf.int = TRUE, palette = c("#FF9E29", "#86AA00"), risk.table = TRUE, risk.table.col = "strata", fun = "cumhaz")

# Assuming you've already loaded the 'survival' library and have ob_data_ans ready.

#Multivariate
fit2 <- survfit(Surv(time = ob_data_ans$adm_to_del_tm, event = rep(1, nrow(ob_data_ans))) ~ age + diff_grav_para, data = ob_data_ans)
ggsurvplot(fit2, pval = TRUE)


#HR

ob_data_hr <- readRDS("OB DATA no NAs.RDS")

library("survival")
library("survminer")
# If your data frame does not already include an 'event' column and you need to create one
# Assuming all non-NA 'adm_to_del_tm' are events (1) and NAs are censored (0)
ob_data_hr$event <- ifelse(is.na(ob_data_hr$adm_to_del_tm), 0, 1)

# Create the survival object
surv_obj <- Surv(time = ob_data_hr$adm_to_del_tm, event = ob_data_hr$event)


# Fitting the Cox model
cox_model <- coxph(Surv(time = ob_data_hr$adm_to_del_tm, event = ob_data_hr$event) ~ gender, data = ob_data_hr)

# Create a survfit object from the Cox model
fit_surv <- survfit(cox_model)

# Plotting the survival curve
ggsurvplot(
    fit = fit_surv,
    data = ob_data_hr,
    pval = TRUE,                # Display p-value of the Cox model
    conf.int = TRUE,            # Display confidence intervals
    risk.table = TRUE,          # Adds a risk table at the bottom
    ggtheme = theme_minimal(),  # Use a minimal theme
    surv.scale = "percent"      # Display survival probabilities in percent
)
ggforest(cox_model)

cox_model_2 <- coxph(Surv(time = ob_data_hr$adm_to_del_tm, event = ob_data_hr$event) ~ gender + age + grav + para, data = ob_data_hr)

ggforest(cox_model_2, data = ob_data_hr)



# Ensure your survival object is set up correctly
surv_obj <- Surv(time = ob_data_hr$adm_to_del_tm, event = rep(1, nrow(ob_data_hr)))

# Fit a Cox model including covariates if available
# Example with a covariate 'gender'
fit_hr <- coxph(surv_obj ~ gender, data = ob_data_hr)

# Plot the survival curve
ggsurvplot(
    fit = fit_surv,                 # Your Cox model
    data = ob_data_hr,
    risk.table = TRUE,           # Adds a risk table
    pval = TRUE,                 # Display p-value of the Cox model
    conf.int = TRUE              # Display confidence intervals
)



#Re-leveling: referencing HR
# Ensure 'intrapartal_conditions' is a factor and set 'None' as the reference level
ob_data_hr$intrapartal_conditions <- relevel(factor(ob_data_hr$intrapartal_conditions), ref = "None")

# Now fit the Cox model
cox_model_ref <- coxph(Surv(time = ob_data_hr$adm_to_del_tm, event = ob_data_hr$event) ~ intrapartal_conditions + age + grav + para + diff_grav_para, data = ob_data_hr)

ggforest(cox_model_ref, data = ob_data_hr)


# Ensure 'intrapartal_events' is a factor and set 'None' as the reference level
ob_data_hr$intrapartal_event <- relevel(factor(ob_data_hr$intrapartal_event), ref = "None")

# Now fit the Cox model
cox_model_ref_2 <- coxph(Surv(time = ob_data_hr$adm_to_del_tm, event = ob_data_hr$event) ~ intrapartal_event + age + grav + para + diff_grav_para, data = ob_data_hr)

ggforest(cox_model_ref_2, data = ob_data_hr)



