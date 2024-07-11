##Group predictors & update
library(dplyr)
library(stringr)

ob_data_hghRsk <- readRDS("High Risk OB.RDS")
library(dplyr)
library(stringr)

ob_data_hghRsk_xtr <- ob_data_hghRsk %>%
    mutate(
        delivery_cnsldt = if_else(str_detect(delivery_method, "Spont"), "Spontaneous", "other"),
        uds_age = case_when(
            age < 15 ~ "lt 15",
            age >= 15 & age < 20 ~ "15-19",
            age >= 20 & age < 25 ~ "20-24",
            age >= 25 & age < 45 ~ "25-44",
            age >= 45 ~ "gt 45",
            TRUE ~ "Unknown"  # handles NA values or any other case not covered
        )
    )
saveRDS(ob_data_hghRsk_xtr, "OB DATA with Consolidated Variables.RDS")
ob_data_hghRsk_vars <- readRDS("OB DATA with Consolidated Variables.RDS") %>% 
    dplyr::select(age, uds_age, grav, para, intrapartal_events, gender, weight, apg1, apg5, gest_age_days, adm_to_del_tm, diff_grav_para, event,
           conditions_cnsldt, lbr_type_cnsldt, presentation_cnsldt, delivery_cnsldt, cluster, hghRsk)
saveRDS(ob_data_hghRsk_vars, "OB DATA with Consolidated & Selective Variables.RDS")

ob_data_vars_factorized <- readRDS("OB DATA with Consolidated & Selective Variables.RDS") %>% 
    mutate(across(where(is.character), as.factor))
saveRDS(ob_data_vars_factorized, "Factorized OB DATA with Selected Variables.RDS")

str(ob_data_vars_factorized)
sum(is.na(ob_data_vars_factorized))
