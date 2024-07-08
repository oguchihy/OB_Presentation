library(survival)
library(survminer)

# Load the dataset
ob_data_compact <- readRDS("OB Data with consolidated Intrapartal conditions.RDS")

# Check and prepare variables
if(!"event" %in% names(ob_data_compact) || !is.numeric(ob_data_compact$adm_to_del_tm)) {
    ob_data_compact$adm_to_del_tm <- as.numeric(ob_data_compact$adm_to_del_tm)
    ob_data_compact$event <- ifelse(!is.na(ob_data_compact$adm_to_del_tm), 1, 0)  # Assuming event is defined like this
}
#!. Intrapartal EVENTS

# Convert intrapartal_events to a factor and adjust the reference level to 'None'
ob_data_compact$intrapartal_events <- factor(ob_data_compact$intrapartal_events)
ob_data_compact$intrapartal_events <- relevel(ob_data_compact$intrapartal_events, ref = "None")

# Fit the Cox proportional hazards model with adjusted reference level
cox_model <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days + weight + diff_grav_para + intrapartal_events,
                   data = ob_data_compact)

# Use survminer's ggforest to visualize the model's output
ggforest(cox_model, data = ob_data_compact)



# Intrapartal conditions
# Convert intrapartal_events to a factor and adjust the reference level to 'None'
ob_data_compact$conditions_cnsldt <- factor(ob_data_compact$conditions_cnsldt)
ob_data_compact$conditions_cnsldt <- relevel(ob_data_compact$conditions_cnsldt, ref = "None")

# Fit the Cox proportional hazards model with adjusted reference level
cox_model <- coxph(Surv(adm_to_del_tm, event) ~ age + grav + para + gest_age_days + weight + diff_grav_para + conditions_cnsldt,
                   data = ob_data_compact)

# Use survminer's ggforest to visualize the model's output
ggforest(cox_model, data = ob_data_compact)
