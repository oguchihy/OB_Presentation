# OB delivery data from NMC 2022, 2023 & 2024Q1

library(readxl)
library(dplyr)
library(purrr)
library(stringr)

# Construct the path to the data file within the subdirectory
data_path <- file.path(Sys.getenv("DROPBOX_PATH"), "CSVS", "OB Service", "NMC OB LOG")


path_to_files <- data_path  # Adjust the path to your directory using set dropbox path Renviron

all_files <- list.files(path = path_to_files, pattern = "\\.xlsx$", full.names = TRUE)

combined_data <- all_files %>%
    map_df(~ read_xlsx(., col_names = TRUE, skip = 1)) %>% 
    janitor::clean_names(.) %>% 
    select(age, grav, para, adm_date,	adm_time, labor_type, anes_type, membrane_rupture, intrapartal_conditions, intrapartal_events, delivery_method, 
           vbac, operative_vaginal, lacerations, baby_sq, delivery_date, delivery_time, gender, weight, apg1, apg5, gest_age, presentation, viability, monitor)

names(combined_data)
str(combined_data)

combined_data_typed <- combined_data %>%
    mutate(
        # Convert character data to numeric
        age = as.numeric(age),
        weight = as.numeric(weight),
        apg1 = as.numeric(apg1),
        apg5 = as.numeric(apg5),
        grav = as.numeric(grav),
        para = as.numeric(para),
        
        # Convert dates to Date objects
        adm_date = as.Date(adm_date, format = "%m/%d/%y"),
        delivery_date = as.Date(delivery_date, format = "%m/%d/%y"),
        
        # Ensure the time is correctly formatted with colons
        adm_time_formatted = if_else(is.na(adm_time), NA, sprintf("%s:%s:00", substr(adm_time, 1, 2), substr(adm_time, 3, 4))),
        delivery_time_formatted = if_else(is.na(delivery_time), NA, sprintf("%s:%s:00", substr(delivery_time, 1, 2), substr(delivery_time, 3, 4))),
        
        # Combine date and formatted time into a single string first
        adm_datetime_str = if_else(is.na(adm_date) | is.na(adm_time_formatted), NA, paste(adm_date, adm_time_formatted)),
        delivery_datetime_str = if_else(is.na(delivery_date) | is.na(delivery_time_formatted), NA, paste(delivery_date, delivery_time_formatted)),
        
        # Convert the combined string to POSIXct
        adm_datetime = if_else(is.na(adm_datetime_str), NA, as.POSIXct(adm_datetime_str, format = "%Y-%m-%d %H:%M:%S")),
        delivery_datetime = if_else(is.na(delivery_datetime_str), NA, as.POSIXct(delivery_datetime_str, format = "%Y-%m-%d %H:%M:%S"))) %>% 
        
    filter(gest_age != "W-210 D-1") %>% 
    filter(gest_age != "W-89  D-0" ) %>%  #remove outlier 
        
    mutate(weeks = as.numeric(str_extract(gest_age, "(?<=W-)\\d+")),  # Extracts digits following 'W-'
        days = as.numeric(str_extract(gest_age, "(?<=D-)\\d+")),  # Extracts digits following 'D-'
        gest_age_days = weeks * 7 + days) %>%
    filter(gest_age_days != 1471) %>% 
    select(-weeks, -days, -adm_time_formatted, -delivery_time_formatted, -adm_datetime_str, -delivery_datetime_str) # Remove intermediate columns if not needed
    
#str(combined_data_typed)
saveRDS(combined_data_typed, "Base OB Data DB.RDS")
