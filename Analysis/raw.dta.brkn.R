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

## Read in data ----

ob_data_raw <- readxl::read_xlsx("Table_6b_Prenatal_Care_3-31-2024.xlsx", sheet = "Results")

## Explore data----
dim(ob_data_raw) #dimensions: number of rows and columns
str(ob_data_raw) # strucure

ob_data_raw %>% slice(1449, 1504, 1572) %>% select(23) ##Explore warnings

## modifications:----

ob_data_cln <- ob_data_raw %>% 
    janitor::clean_names() %>%  # take care of column names
    mutate(delivery_doc_date_dt=anydate(delivery_doc_date_dt),
           fetal_demise_date_dt = anydate(fetal_demise_date_dt),
           spontaneous_abortion_date_dt = anydate(spontaneous_abortion_date_dt)) # take care of badly formed dates

saveRDS(ob_data_cln, "OB Data, Raw.RDS")

## Explore ----

ob_data_cln_1 <- readRDS("OB Data, Raw.RDS") %>% 
    mutate(delivery_birthweight_num2 = if_else(
        str_detect(delivery_birthweight_num2, ","), # Condition: Check for comma
        # Use parse_number on the substring after the last comma
        parse_number(sub(".*,\\s*", "", as.character(delivery_birthweight_num2))),
        # Directly use parse_number if no comma is present
        parse_number(as.character(delivery_birthweight_num2))
    )) %>% 
    mutate(delivery_birthweight_num2 = if_else(delivery_birthweight_num2 == 420, 4200, delivery_birthweight_num2))


# ob_data_cln_1 %>%  DataExplorer::introduce()
# 
# ob_data_cln_1 %>% explore()
#levels(factor(ob_data_cln_1$delivery_birthweight_num2))

hist(ob_data_cln_1$age)

range(ob_data_cln_1$age)

summary(ob_data_cln_1$age)

hist(ob_data_cln_1$delivery_birthweight_num2)

range(ob_data_cln_1$delivery_birthweight_num2, na.rm = TRUE)

ob_data_cln_1 %>% filter(age >= 46)
ob_data_cln_1 %>% filter(age >= 50)

##Data suspect; has pregnancies at 74 yrs of age. 
##Exploration leads to ICD10 code of z33.1 New report run off server for z33.1
##identify affected patients
z33_1_pts <- read_csv("z33.1 pregnancy patients.csv", col_names = TRUE) %>% 
    janitor::clean_names() %>% 
    filter(icd_code =="Z33.1") %>% 
    mutate(across(c(dob, date_onset, diagn_date_time), ~anydate(.))) %>% 
    select(-x1) %>% 
    mutate(age = parse_number(age)) %>% 
    filter(diagn_date_time>="2022-01-01" & diagn_date_time <="2024-03-31")

##Remove  the z33.1 patients using anti-join

ob_data_scraped <- anti_join(ob_data_cln_1, z33_1_pts, join_by("episode_indicator_code"=="icd_code")) %>% 
    select(where(~ !all(is.na(.))))
#names(ob_data_scraped)
# [1] "name"                           "race"                           "ethnicity"                      "language"                       "phone"                         
# [6] "address_1"                      "city"                           "state"                          "zip"                            "mrn"                           
# [11] "sex_at_birth"                   "date_of_birth_dt"               "age"                            "uds_age"                        "usual_provider"                
# [16] "most_recent_rendering_provider" "most_recent_rendering_location" "inactive"                       "episode_indicator_date_dt"      "episode_indicator_code"        
# [21] "gestational_wks"                "preg_start_edc_dt"              "preg_start_type"                "est_delivery_date_dt"           "est_delivery_type"             
# [26] "delivery_date_dt"               "delivery_birthweight_num2"      "delivery_doc_date_dt"           "delivery_doc_code"              "fetal_demise_date_dt"          
# [31] "fetal_demise_code"              "spontaneous_abortion_date_dt"   "spontaneous_abortion_code"      "initial_visit_dt"               "initial_trimester"             
# [36] "initial_trimester_location"   

#ob_data_scraped %>% explore()

# Function to convert "weeks_days" format to total days ----
convert_gest_age_to_days <- function(weeks_days) {
    parts <- strsplit(weeks_days, "_")
    weeks <- as.numeric(sapply(parts, `[`, 1))
    days <- as.numeric(sapply(parts, `[`, 2))
    total_days <- weeks * 7 + days
    return(total_days)
}

ob_data_scraped_1 <- ob_data_scraped %>% 
    mutate(gestational_days = convert_gest_age_to_days(gestational_wks))

ob_data_slim <- ob_data_scraped_1%>% 
    select(city, zip, age, uds_age, episode_indicator_date_dt, episode_indicator_code, gestational_wks, gestational_days, preg_start_edc_dt, 
           preg_start_type, est_delivery_date_dt, delivery_date_dt, delivery_birthweight_num2, delivery_doc_date_dt, delivery_doc_code, 
           spontaneous_abortion_date_dt, spontaneous_abortion_code, initial_trimester) %>% 
    mutate(across(where(is.POSIXct), as.Date))


## Models and Analysis----
library(dplyr)
library(stringr)
library(ggplot2)

#Prepare data for modelling. Key: Use only data where birth wt is known.
mdl.ds_1 <- ob_data_slim %>%
    filter(!is.na(delivery_birthweight_num2))

# First, attempt to convert delivery_birthweight_num2 to numeric to identify NAs
mdl.ds_1$delivery_birthweight_num2 <- as.numeric(gsub("[^0-9.]", "", mdl.ds_1$delivery_birthweight_num2))

# Correct city name variations in the cleaned data
ob_data_clean_2 <- mdl.ds_1 %>%
    mutate(city = str_replace_all(city, c("Salianas" = "Salinas", "Salsinas" = "Salinas")))

# Convert initial_trimester to a factor
ob_data_clean_2$initial_trimester <- factor(ob_data_clean_2$initial_trimester)

# Reduce city to top 12, grouping the rest as 'Other'
top_cities <- ob_data_clean_2 %>%
    count(city, sort = TRUE) %>%
    top_n(12, wt = n) %>%
    pull(city)

ob_data_clean_3 <- ob_data_clean_2 %>%
    mutate(city = ifelse(city %in% top_cities, city, 'Other'),
           city = factor(city)) # Ensure 'city' is a factor

# Reduce zip to top 10, grouping the rest as 'Other'
top_zips <- ob_data_clean_3 %>%
    count(zip, sort = TRUE) %>%
    top_n(10, wt = n) %>%
    pull(zip)

ob_data_clean_4 <- ob_data_clean_3 %>%
    mutate(zip = ifelse(zip %in% top_zips, zip, 'Other'),
           zip = factor(zip)) # Ensure 'zip' is a factor

ob_data_for_mdl <-ob_data_clean_4 

saveRDS(ob_data_for_mdl, "OB DATA for Modelling on Birth Weight.RDS")

##Get master df----
mdl.ds <- readRDS("OB DATA for Modelling on Birth Weight.RDS")

##LMN ----
# Fit the multiple linear regression model
model <- lm(delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester, data = mdl.ds, na.action = na.exclude)
summary(model)

# Diagnostic plots for the model
par(mfrow = c(2, 2))
plot(model)


# Residuals by initial_trimester plot
mdl.ds$residuals <- residuals(model)

ggplot(mdl.ds, aes(x = initial_trimester, y = residuals)) +
    geom_boxplot() +
    labs(title = "Residuals by Initial Trimester", x = "Initial Trimester", y = "Residuals")



##GLM

# Load the necessary package for GLM
library(stats)

# Fit a GLM
glm_model <- glm(delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester, 
                 data = mdl.ds, family = gaussian())

# Summary of the GLM
summary(glm_model)


##Random Forest----

# Load necessary package
library(randomForest)

mdl.ds.rf <- mdl.ds %>%
    filter(!is.na(delivery_birthweight_num2) & !is.na(age) & !is.na(gestational_days) & 
               !is.na(city) & !is.na(zip) & !is.na(initial_trimester))


# Convert factors to character to avoid automatic conversion to dummy variables
mdl.ds.rf$city <- as.character(mdl.ds.rf$city)
mdl.ds.rf$zip <- as.character(mdl.ds.rf$zip)
mdl.ds.rf$initial_trimester <- as.character(mdl.ds.rf$initial_trimester)

# Fit a Random Forest model
rf_model <- randomForest(delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester,
                         data = mdl.ds.rf, ntree = 500, importance = TRUE)

# Summary of the Random Forest model
print(rf_model)

# Variable importance
importance(rf_model)
varImpPlot(rf_model)


##GBM----
mdl.ds.gbm <- mdl.ds %>%
    filter(!is.na(delivery_birthweight_num2) & !is.na(age) & !is.na(gestational_days) & 
               !is.na(city) & !is.na(zip) & !is.na(initial_trimester))


# Convert character variables to factors
mdl.ds.gbm$city <- as.factor(mdl.ds.gbm$city)
mdl.ds.gbm$zip <- as.factor(mdl.ds.gbm$zip)
mdl.ds.gbm$initial_trimester <- as.factor(mdl.ds.gbm$initial_trimester)

# Attempt to fit the GBM model again
library(gbm)

gbm_model <- gbm(delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester,
                 data = mdl.ds.gbm, 
                 distribution = "gaussian",
                 n.trees = 5000,
                 interaction.depth = 3,
                 shrinkage = 0.01,
                 cv.folds = 5,
                 verbose = FALSE) # Add verbose = FALSE to reduce output verbosity

# Print a summary of the GBM model
summary(gbm_model)

# Plot the variable importance
gbm.plot(gbm_model, n.plots = 3, write.title = FALSE, plot.layout = c(3, 1))


# ##XGboost ----
# library(xgboost)
# library(dplyr)
# 
# # Filtering the dataset to remove rows with NA values in crucial columns
# mdl_gbst_filtered <- mdl.ds %>%
#     filter(!is.na(delivery_birthweight_num2) & !is.na(age) & !is.na(gestational_days) & 
#                !is.na(city) & !is.na(zip) & !is.na(initial_trimester))
# 
# # Now, to handle categorical variables ('city' and 'zip' are categorical),
# # convert them into numeric codes if not already done. This is a simplistic approach,
# # and you might consider more sophisticated encoding methods depending on your analysis needs.
# 
# # Converting factors to numeric codes
# mdl_gbst_filtered$city <- as.integer(as.factor(mdl_gbst_filtered$city))
# mdl_gbst_filtered$zip <- as.integer(as.factor(mdl_gbst_filtered$zip))
# mdl_gbst_filtered$initial_trimester <- as.integer(as.factor(mdl_gbst_filtered$initial_trimester))
# 
# # Ensure that the entire dataset is numeric
# mdl_gbst_filtered <- data.frame(sapply(mdl_gbst_filtered, as.numeric))
# 
# # Check for any remaining non-numeric columns just to be safe
# if(any(sapply(mdl_gbst_filtered, class) == "character")) {
#     stop("Non-numeric columns found in the dataset.")
# }
# 
# # Prepare labels
# labels <- mdl_gbst_filtered$delivery_birthweight_num2
# 
# # Prepare the matrix excluding the target variable 'delivery_birthweight_num2'
# data_matrix <- as.matrix(mdl_gbst_filtered[, -which(names(mdl_gbst_filtered) == "delivery_birthweight_num2")])
# 
# # Create the XGBoost DMatrix
# dtrain <- xgb.DMatrix(data = data_matrix, label = labels)
# 
# # Now dtrain is ready for use with XGBoost training functions without encountering the 'data' class error.
# 
# # Step 4: Define model parameters
# params <- list(
#     booster = "gbtree",
#     objective = "reg:squarederror",
#     eta = 0.3,
#     max_depth = 6,
#     subsample = 0.8,
#     colsample_bytree = 0.8,
#     eval_metric = "rmse"
# )
# 
# # Step 5: Define the evaluation metric within the watchlist
# watchlist <- list(train = dtrain)  # Note: Ideally, include a validation set as well
# 
# # Define the evaluation set for early stopping
# # 'watchlist' should be a list of pairs (name, DMatrix)
# evals <- list(train = dtrain)  # Using the training set for demonstration; ideally, include a validation set
# 
# xgb_model <- xgb.train(
#     params = params,
#     data = dtrain,  # Use 'data = dtrain' to correctly pass the DMatrix
#     nrounds = 100,
#     evals = evals,  # Ensure 'evals' references your evaluation set properly
#     early_stopping_rounds = 10,
#     verbose = 1
# )
# 
# 
# 
# 
# # Display the model's best iteration (if early stopping is triggered)
# cat("\nBest Iteration: ", xgb_model$best_iteration, "\n")
# 

#

##LightGBM ----
# Ensure LightGBM is installed
# install.packages("lightgbm")

library(lightgbm)

mdl.ds.ltgbm <- mdl.ds %>%
    filter(!is.na(delivery_birthweight_num2) & !is.na(age) & !is.na(gestational_days) & 
               !is.na(city) & !is.na(zip) & !is.na(initial_trimester))

# Prepare data for LightGBM
data_matrix <- lgb.Dataset(data = as.matrix(mdl.ds.ltgbm[, -which(names(mdl.ds.ltgbm) == "delivery_birthweight_num2")]),
                           label = mdl.ds.ltgbm$delivery_birthweight_num2)

# Set parameters
params <- list(objective = "regression",
               metric = "l2",
               num_leaves = 31,
               learning_rate = 0.05,
               feature_fraction = 0.9,
               bagging_fraction = 0.8,
               bagging_freq = 5)

# Train the model
lgb_model <- lgb.train(params = params,
                       data = data_matrix,
                       nrounds = 100,
                       verbose = 0)

# Summary of the model
print(lgb_model)

# Plotting the importance of each feature
lgb.importance(lgb_model) %>%
    lgb.plot.importance()



##Clustering ----

library(dplyr)
library(cluster)
library(ggplot2)

# Assuming mdl.ds is the initial dataset

# Select relevant variables and scale
mdl_clustering <- mdl.ds %>%
    select(age, gestational_days, delivery_birthweight_num2) %>%
    na.omit()

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
    summarise(across(c(age, gestational_days, delivery_birthweight_num2), mean, na.rm = TRUE))

print(cluster_summary)

ggplot(mdl_clustering, aes(x = gestational_days, y = delivery_birthweight_num2, color = cluster)) +
    geom_point(alpha = 0.5) +
    theme_minimal() +
    labs(title = "Cluster Analysis on Gestational Days and Birth Weights",
         x = "Gestational Days", y = "Delivery Birthweight")







# Assuming mdl.ds is your dataset

# Select relevant variables and scale
mdl_clustering <- mdl.ds %>%
    select(age, gestational_days, delivery_birthweight_num2) %>%
    na.omit()

mdl_clustering_scaled <- scale(mdl_clustering)

# Perform K-means clustering
set.seed(123)  # Ensure reproducibility
k <- 3  # Number of clusters
kmeans_result <- kmeans(mdl_clustering_scaled, centers = k, nstart = 25)

# Add the cluster assignments to the original dataset for visualization
mdl_clustering$cluster <- as.factor(kmeans_result$cluster)


library(ggplot2)

ggplot(mdl_clustering, aes(x = age, y = gestational_days, color = cluster)) +
    geom_point(alpha = 0.6) +  # Plot points with cluster-based coloring
    theme_minimal() +
    labs(title = "Age vs. Gestational Days by Cluster",
         x = "Age",
         y = "Gestational Days") +
    scale_color_discrete(name = "Cluster")  # Add legend for clusters


ggplot(mdl_clustering, aes(x = age, y = delivery_birthweight_num2, color = cluster)) +
    geom_point(alpha = 0.6) +  # Plot points with cluster-based coloring
    theme_minimal() +
    labs(title = "Age vs. Delivery Birthweight by Cluster",
         x = "Age",
         y = "Delivery Birthweight") +
    scale_color_discrete(name = "Cluster")  # Add legend for clusters

