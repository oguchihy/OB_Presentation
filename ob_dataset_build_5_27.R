# BUILD OB delivery data from NMC 2022, 2023 & 2024->

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
# Get & basic data ----


# Construct the path to the data file within the subdirectory
#data_path <- file.path(Sys.getenv("DROPBOX_PATH"), "OB DATA ANALYTICS")

data_path <- "home/onkwocha/OHN/OB DATA ANALYTICS"
path_to_files <- data_path  # Adjust the path to your directory using set dropbox path Renviron

all_files <- list.files(path = "C:/Users/onkwocha/Dropbox/My PC (DESKTOP-J3JQIK3)/Desktop/LocalForGDrive/OB_DATA/orig_files", pattern = "\\.xlsx$", full.names = TRUE)

combined_data <- all_files %>%
    map_df(~ read_xlsx(., col_names = TRUE, skip = 1)) %>% 
    janitor::clean_names(.) %>% 
    select(postal_code, age, grav, para, adm_date,	adm_time, labor_type, membrane_rupture, intrapartal_conditions, intrapartal_events, delivery_method, 
           baby_sq, delivery_date, delivery_time, baby_sq, gender, weight, apg1, apg5, gest_age, presentation)

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
    select(-weeks, -days, -adm_time_formatted, -delivery_time_formatted, -adm_datetime_str, -delivery_datetime_str) %>%  # Remove intermediate columns if not needed

    mutate(postal_code = if_else(is.na(postal_code), "00000", postal_code)) %>%
    rename(zip = postal_code) %>%
    group_by(zip) %>%
    mutate(zip = case_when(n() >= 5 ~ as.character(zip), TRUE ~ "99999")) %>%
    ungroup() 


#str(combined_data_typed)
saveRDS(combined_data_typed, "Base OB Data DB.RDS")

# Create dervived columns ----

ob.data.db <- readRDS("Base OB Data DB.RDS") 

# Add features calc vars ----

ob.data.augm <- ob.data.db %>% 
    mutate(
        # calculate time from admission to Del in hours
        adm_to_del_tm = as.double(difftime(delivery_datetime, adm_datetime, units = "hours")),
        adm_to_del_tm = round(adm_to_del_tm, 0),
        adm_to_del_tm = as.numeric(adm_to_del_tm),
        #Calculate Gravida-Parity difference
        diff_grav_para = grav-para,
        diff_grav_para = if_else(diff_grav_para <=0, 1, diff_grav_para)
    )

saveRDS(ob.data.augm, "Full DB OB DATA.RDS")

# Intrapartal EVents and Intrapartal conditions ----
ob_data_ans <- readRDS("Full DB OB DATA.RDS") 
ob_data_ans$membrane_rupture<- ifelse(is.na(ob_data_ans$membrane_rupture) | ob_data_ans$membrane_rupture == "N/A", "None", ob_data_ans$membrane_rupture)
ob_data_ans$intrapartal_events<- ifelse(is.na(ob_data_ans$intrapartal_events) | ob_data_ans$intrapartal_events == "N/A", "None", ob_data_ans$intrapartal_events)
ob_data_ans$intrapartal_conditions<- ifelse(is.na(ob_data_ans$intrapartal_conditions) | ob_data_ans$intrapartal_conditions == "N/A", "None", ob_data_ans$intrapartal_conditions)

# Deal with NAs ----
## adm_to_del_tm ----
cutoff <- quantile(ob_data_ans$adm_to_del_tm, 0.99, na.rm = TRUE)  # 99th percentile as the cutoff
ob_data_ans$adm_to_del_tm <- pmin(ob_data_ans$adm_to_del_tm, cutoff)

# Calculate the median of the 'adm_to_del_tm' while ignoring NA values
median_adm_to_del_tm <- median(ob_data_ans$adm_to_del_tm, na.rm = TRUE)

# Impute NA values with the calculated median
ob_data_ans$adm_to_del_tm[is.na(ob_data_ans$adm_to_del_tm)] <- median_adm_to_del_tm

saveRDS(ob_data_ans, "OB DATA no NAs.RDS")

##Remove unneeded vars source of NAs----
ob_data_reg <- readRDS("OB DATA no NAs.RDS") %>% 
  #get event and time (duration) for TTE analysis
  mutate(event = if_else(is.na(delivery_time)|delivery_time =="",0,1),
         time= as.numeric(adm_to_del_tm))
ob_data_slim <- ob_data_reg 
ob_data_slim$adm_datetime <- NULL
ob_data_slim$delivery_datetime <- NULL
ob_data_slim$adm_time <- NULL
ob_data_slim$delivery_time <- NULL
ob_data_slim$gender[is.na(ob_data_slim$gender)] <- "unknown"

## Numerical columns imputation with median ----
numerical_cols <- sapply(ob_data_slim, is.numeric)
ob_data_slim[numerical_cols] <- lapply(ob_data_slim[numerical_cols], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
## Categorical columns imputation with mode ----
categorical_cols <- sapply(ob_data_slim, is.character)
ob_data_slim[categorical_cols] <- lapply(ob_data_slim[categorical_cols], function(x) ifelse(is.na(x), names(which.max(table(x))), x))
colSums(is.na(ob_data_slim))
saveRDS(ob_data_slim, "Cleaned OB Dataset Ready for Analysis.RDS")


ob_data_mdl <- readRDS("Cleaned OB Dataset Ready for Analysis.RDS") 

# Convert adm_to_del_tm_cat to a factor ----
ob_data_del_tm <- ob_data_mdl %>%
    mutate(adm_to_del_tm_cat = factor(case_when(
        adm_to_del_tm < 12 ~ "Under 12 hours",
        adm_to_del_tm >= 12 & adm_to_del_tm < 24 ~ "13-24 hours",
        adm_to_del_tm >= 24 & adm_to_del_tm < 48 ~ "25-48 hours",
        adm_to_del_tm >= 48 ~ "Over 48 hours"
    )))

# UDS-Age ----
ob_data_uds <- ob_data_del_tm %>% 
mutate(
    uds_age = case_when(
    age < 15 ~ "lt 15",
    age >= 15 & age < 20 ~ "15-19",
    age >= 20 & age < 25 ~ "20-24",
    age >= 25 & age < 45 ~ "25-44",
    age >= 45 ~ "gt 45",
    TRUE ~ "Unknown"  # handles NA values or any other case not covered
  )
)


# Add HighRisk OB ----
ob_data_hghRsk <- ob_data_uds %>%
    mutate(
        hghRsk = (
          case_when(
            age > 35 ~ "yes",  # Age greater than 35
            str_detect(intrapartal_conditions, "Preeclampsia") ~ "yes",  # Presence of Preeclampsia
            baby_sq > 1 ~ "yes",  # Multiple births
            !str_detect(presentation, "Vertex") ~ "yes",  # Non-Vertex presentation
            str_detect(intrapartal_conditions, "None") & !str_detect(intrapartal_conditions, "Preeclampsia") ~ "no",  # Only 'None' without Preeclampsia
            TRUE ~ "no"  # Default case
            )
        ),
        hghRsk = as.factor(hghRsk)
    )
  #mutate(across(where(is.character), as.factor))
saveRDS(ob_data_hghRsk, "OB_data with High Risk OB.RDS")


# Consolidate conditions and other variables

##Consolidate intrapartalconditions and events ----

ob_data_cnsldt <- readRDS("OB_data with High Risk OB.RDS") %>% 
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
                                     if_else(conditions_cnsldt == "Prolonged ROM, Hemorrhage", "Prolonged ROM", conditions_cnsldt))) %>% 
  ## Consolidate labor type ----
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
    
    lbr_type_cnsldt = if_else(lbr_type_cnsldt == "Induced, Suppressed", "Suppresses",lbr_type_cnsldt),
    lbr_type_cnsldt = if_else(lbr_type_cnsldt == "Spontaneous, Induced", "Not Applicable",lbr_type_cnsldt),
    lbr_type_cnsldt = if_else(lbr_type_cnsldt == "Spontaneous, Not Applicable", "Not Applicable",lbr_type_cnsldt),
    lbr_type_cnsldt = if_else(lbr_type_cnsldt == "Induced, Not Applicable", "Not Applicable",lbr_type_cnsldt),
    
    ## Consolidate presentation ----
    presentation_cnsldt=if_else(presentation == "Vertex", "Vertex", "Breech"),
    
    ## Consolidate delivery type----
    del_method_cnsldt = case_when(
      str_detect(delivery_method, "Spont") |str_detect(delivery_method, "Vacuum") | str_detect(delivery_method, "Fetal") ~ "Vaginal",
      str_detect(delivery_method, "Prim") ~ "C/S, Primary",
      str_detect(delivery_method, "Repeat") | str_detect(delivery_method, "vBAC") ~ "C/S, Repeat" #,
      #TRUE ~ as.character(delivery_method)
      )
      
      
  )

saveRDS(ob_data_cnsldt, "OB Data with consolidated Intrapartal conditions, labor type and presentation.RDS")


# Cluster var----
ob_data_clstr <- readRDS("OB Data with consolidated Intrapartal conditions, labor type and presentation.RDS") %>% 
  dplyr::select(gest_age_days, weight)
ob_data_clstr_scaled <- scale(ob_data_clstr)

## Cluster Analysis ----
set.seed(123)

# Perform K-means clustering
k <- 3
kmeans_result <- kmeans(ob_data_clstr_scaled, centers = k, nstart = 25)

# Add the cluster assignments to the original dataset
ob_data_cluster <- readRDS("OB Data with consolidated Intrapartal conditions, labor type and presentation.RDS")
ob_data_cluster$cluster <- as.factor(kmeans_result$cluster)

saveRDS(ob_data_cluster, "Working OB Dataset.RDS")

# Factor char vars----
ob_data_factored <- readRDS("Working OB Dataset.RDS") %>% 
  mutate(across(where(is.character), as.factor))


saveRDS(ob_data_factored, "All Factored Complete Ready OB Dataset for Analytics.RDS")
