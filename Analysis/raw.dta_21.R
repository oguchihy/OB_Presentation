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
    ))


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
    filter(!is.na(delivery_birthweight_num2),
           gestational_days>140) # remove gestationa age <20 weeks (140 days)

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
    mutate(zip = substr(as.character(zip), 1, 5)) %>% 
    count(zip, sort = TRUE) %>%
    top_n(10, wt = n) %>%
    pull(zip)

ob_data_clean_4 <- ob_data_clean_3 %>%
    mutate(zip = ifelse(zip %in% top_zips, zip, 'Other'),
           zip = factor(zip)) # Ensure 'zip' is a factor

ob_data_for_mdl <-ob_data_clean_4 

#bgs <- mdl.ds %>% filter(gestational_days<=200)

saveRDS(ob_data_for_mdl, "OB DATA for Modelling on Birth Weight.RDS")

#Start here: Get master df----

mdl.ds <- readRDS("OB DATA for Modelling on Birth Weight.RDS")
saveRDS(mdl.ds, "mdl.ds.RDS")

# Moments ----
library(dplyr)
library(moments)  # Load moments package for skewness and kurtosis

# Load your data set
mdl.moments <- readRDS("mdl.ds.RDS") %>%
    # Convert gestational days to full weeks and round birth weight to the nearest integer
    mutate(
        gestational_weeks = floor(gestational_days / 7),  # Assuming 'gestational_days' is the correct column name
        delivery_birthweight_num2 = round(delivery_birthweight_num2)  # Assuming this is the column name for birth weight
    ) %>%
    # Select only numeric columns
    select_if(is.numeric)
saveRDS(mdl.moments, "Moments.RDS")

# Calculate statistical moments including median
summary_stats <- sapply(mdl.moments, function(x) {
    c(median = median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      variance = var(x, na.rm = TRUE),
      skewness = skewness(x, na.rm = TRUE),
      kurtosis = kurtosis(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE))
})

# Convert to a more readable and formatted data frame
summary_stats_df <- as.data.frame(t(summary_stats))
colnames(summary_stats_df) <- c("Median", "Mean", "Variance", "Skewness", "Kurtosis", "SD")

saveRDS(summary_stats_df, "Moments Data.RDS")
# Format the output to be more readable
print(summary_stats_df, digits = 2)

#Plot  SD ----
library(ggplot2)

# Create distribution plots for each numeric variable in mdl.moments

# First, we need to reshape your data into a long format suitable for ggplot2
mdl.moments_long <- mdl.moments %>% 
    select(-gestational_days) %>% 
    tidyr::pivot_longer(., cols = everything())

# Now create the plot
ggplot(mdl.moments_long, aes(x = value)) + 
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +  # Histogram for distribution
    facet_wrap(~ name, scales = "free") +  # Faceting by variable name
    labs(title = "Distribution of Numeric Variables",
         x = "Value",
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve x-axis label readability



##LMN ----
# Fit the multiple linear regression model
mdl.ds.lm <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")

model <- lm(birth_wt ~ age + gest_age + city + zip +initial_trimester + uds_age, data = mdl.ds.lm, na.action = na.exclude)
summary(model)

# Diagnostic plots for the model
par(mfrow = c(2, 2))
plot(model)


# Residuals by initial_trimester plot
mdl.ds.lm$residuals <- residuals(model)

ggplot(mdl.ds.lm, aes(x = initial_trimester, y = residuals)) +
    geom_boxplot() +
    labs(title = "Residuals by Initial Trimester", x = "Initial Trimester", y = "Residuals")



##GLM ----

# Load the necessary package for GLM
library(stats)
mdl.ds.glm <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")
# Fit a GLM
glm_model <- glm(birth_wt ~ age + gest_age + city + zip +initial_trimester + uds_age, data = mdl.ds.glm, family = gaussian())

# Summary of the GLM
summary(glm_model)


##Random Forest----

# Load necessary package
library(randomForest)

mdl.ds.rf <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")

# Convert factors to character to avoid automatic conversion to dummy variables
mdl.ds.rf$city <- as.character(mdl.ds.rf$city)
mdl.ds.rf$zip <- as.character(mdl.ds.rf$zip)
mdl.ds.rf$initial_trimester <- as.character(mdl.ds.rf$initial_trimester)
mdl.ds.rf$uds_age <- as.character(mdl.ds.rf$uds_age)

# Fit a Random Forest model
rf_model <- randomForest(birth_wt ~ age + gest_age + city + zip +initial_trimester + uds_age, data = mdl.ds.rf, ntree = 500, importance = TRUE)

# Summary of the Random Forest model
print(rf_model)

# Variable importance
importance(rf_model)
varImpPlot(rf_model)


##GBM----
mdl.ds.gbm <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")

# Convert character variables to factors
mdl.ds.gbm$city <- as.factor(mdl.ds.gbm$city)
mdl.ds.gbm$zip <- as.factor(mdl.ds.gbm$zip)
mdl.ds.gbm$initial_trimester <- as.factor(mdl.ds.gbm$initial_trimester)
mdl.ds.gbm$uds_age <- as.factor(mdl.ds.gbm$uds_age)

# Attempt to fit the GBM model again
library(gbm)

gbm_model <- gbm(birth_wt ~ age + gest_age + city + zip +initial_trimester + uds_age,
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
plot(gbm_model, n.plots = 3, write.title = FALSE, plot.layout = c(3, 1))


##XGboost ----
# Load the required library
library(xgboost)

mdl.ds.xgb <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")
mdl_gbst_filtered <- mdl.ds.xgb

# Convert initial_trimester to numeric
mdl_gbst_filtered$initial_trimester <- as.numeric(as.character(mdl_gbst_filtered$initial_trimester))

# Define the model formula
formula <- birth_wt ~ age + gest_age + city + zip +initial_trimester + uds_age

# Create a matrix for the predictors
X <- model.matrix(formula, data = mdl_gbst_filtered)

# Extract the response variable
y <- mdl_gbst_filtered$birth_wt

# Train the XGBoost model
xgb_model <- xgboost(data = X, label = y, nrounds = 100, objective = "reg:squarederror")

# Print the summary of the model
summary(xgb_model)
# Plot feature importance
#xgb.plot.importance(model = xgb_model)# Extract feature importance
importance <- xgb.importance(model = xgb_model)

# Filter feature importance data
importance_filtered <- importance[importance$Gain >= 0.02, ]

# Plot filtered feature importance using ggplot2 with horizontal bars
ggplot(data = importance_filtered, aes(x = Gain, y = Feature)) +
    geom_col() +
    labs(x = "Gain Importance", y = "Feature", title = "Feature Importance (Gain >= 0.02)")

# #

##LightGBM ----
# Ensure LightGBM is installed
# install.packages("lightgbm")

library(lightgbm)

mdl.ds.ltgbm <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")

# Prepare data for LightGBM
data_matrix <- lgb.Dataset(data = as.matrix(mdl.ds.ltgbm[, -which(names(mdl.ds.ltgbm) == "birth_wt")]),
                           label = mdl.ds.ltgbm$birth_wt)

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
mdl_clustering <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS") %>% 
    select(age, gest_age, birth_wt) %>%
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
    summarise(across(c(age, gest_age, birth_wt), mean, na.rm = TRUE))

print(cluster_summary)

ggplot(mdl_clustering, aes(x = gest_age, y = birth_wt, color = cluster)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = c(3000, 4000), linetype = "dashed", color = "red") +
    geom_vline(xintercept = 259, linetype = "dashed", color = "blue") +
    theme_minimal() +
    scale_x_continuous(
        name = "Gestational Age in completed Weeks",  # Rename the x-axis to "Weeks"
        breaks = seq(0, max(mdl_clustering$gest_age), by = 7),  # Set breaks every 7 days
        labels = function(x) floor(x / 7)  # Convert days to floor weeks
    ) +
    labs(title = "Cluster Analysis on Gestational Age and Birth Weights",
         x = "Gestational Age", y = "Delivery Birthweight")

# Assuming mdl.ds is your dataset

# Select relevant variables and scale
mdl_clustering_var <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS") %>%
    select(age, gest_age, birth_wt) %>%
    na.omit()

mdl_clustering_scaled <- scale(mdl_clustering_var)

# Perform K-means clustering
set.seed(123)  # Ensure reproducibility
k <- 3  # Number of clusters
kmeans_result <- kmeans(mdl_clustering_scaled, centers = k, nstart = 25)

# Add the cluster assignments to the original dataset for visualization
mdl_clustering$cluster <- as.factor(kmeans_result$cluster)
saveRDS(mdl_clustering, "Model Data for Clustering.RDS")


library(ggplot2)

ggplot(mdl_clustering, aes(x = age, y = gest_age, color = cluster)) +
    geom_point(alpha = 0.6) +  # Plot points with cluster-based coloring
    theme_minimal() +
    scale_y_continuous(
        name = "Gestational Age (Wks)",  # Rename the x-axis to "Weeks"
        breaks = seq(0, max(mdl_clustering$gest_age), by = 7),  # Set breaks every 7 days
        labels = function(x) floor(x / 7)  # Convert days to floor weeks
    ) +
    labs(title = "Age vs. Gestational Weeks by Cluster",
         x = "Age",
         y = "Gestational Weeks") +
    scale_color_discrete(name = "Cluster")  # Add legend for clusters


ggplot(mdl_clustering, aes(x = gest_age, y = birth_wt, color = cluster)) +
    geom_point(alpha = 0.6) +  # Plot points with cluster-based coloring
    theme_minimal() +
    scale_x_continuous(
        name = "Gestational Age in Weeks",  # Rename the x-axis to "Weeks"
        breaks = seq(0, max(mdl_clustering$gest_age), by = 7),  # Set breaks every 7 days
        labels = function(x) floor(x / 7)  # Convert days to floor weeks
    ) +
    labs(title = "Age vs. Delivery Birthweight by Cluster",
         x = "Gestational Days",
         y = "Delivery Birthweight") +
    scale_color_discrete(name = "Cluster")  # Add legend for clusters



##Other Analysis----

##Dalex et al----


# Load the packages
library(DALEX)
library(pdp)
library(iml)
library(randomForest)
library(ggplot2)

# Load or create the filtered data
ob_data_filtered <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")

# Train the Random Forest model
rf_model <- randomForest(birth_wt ~ age + gest_age + city + zip +initial_trimester + uds_age, data = ob_data_filtered)

# Save the trained model
saveRDS(rf_model, "rf_model.RDS")

# Create explainer object
rf_explainer <- DALEX::explain(rf_model, 
                               data = ob_data_filtered[, c("age", "gest_age", "city", "zip", "initial_trimester", "uds_age")], 
                               y = ob_data_filtered$birth_wt)

# Save the explainer object
saveRDS(rf_explainer, "rf_explainer.RDS")

# Calculate feature importance
fi <- model_parts(rf_explainer, type = "raw")

# Plot feature importance
plot(fi)

# Obtain prediction breakdown for a sample observation
bd <- predict_parts(rf_explainer, new_observation = ob_data_filtered[1, ], type = "break_down")
plot(bd)

# Ensure 'fastshap' is installed
if (!requireNamespace("fastshap", quietly = TRUE)) install.packages("fastshap")

# Define a prediction wrapper compatible with your Random Forest model
pred_wrapper <- function(model, newdata) {
    # Ensure newdata is a dataframe, as randomForest predict() expects a dataframe
    if (is.matrix(newdata)) {
        newdata <- as.data.frame(newdata)
    }
    
    # Assuming 'newdata' is already preprocessed (e.g., categorical variables are factors)
    predict(model, newdata)
}

# Load the prepared data
X_prepared <- ob_data_filtered

# Compute SHAP values
shap_values <- fastshap::explain(rf_model, X = X_prepared, pred_wrapper = pred_wrapper)


saveRDS(shap_values, "shap_values.RDS")

library(tidyverse)

# Assuming `shap_values` is your matrix/dataframe of SHAP values
# And `X_prepared` is the dataframe used for the prediction that corresponds to the SHAP values
shap_long <- as.data.frame(shap_values) %>%
    mutate(observation = row_number()) %>%
    pivot_longer(-observation, names_to = "feature", values_to = "shap_value")

saveRDS(shap_long, "shap_long.RDS")

# Adding actual feature values for each observation might provide more context in the plots
for (feature in names(X_prepared)) {
    shap_long <- shap_long %>%
        mutate(!!sym(feature) := X_prepared[observation, feature])
}

library(ggplot2)

# Assuming `shap_values` and `X_prepared` are already defined
# Convert SHAP values to a long-format dataframe
shap_long <- reshape2::melt(shap_values) %>%
    mutate(feature = factor(Var2, levels = unique(Var2)), # Ensuring features are factors for plotting
           observation = Var1) %>%
    select(-Var1, -Var2)
saveRDS(shap_long, "shap_long.RDS")


# Add actual 'age' values from `X_prepared` for plotting (assuming 'age' is in the correct order)
shap_long$age <- X_prepared$age[shap_long$observation]

# Plotting SHAP values for 'age'
ggplot(shap_long[shap_long$feature == "age", ], aes(x = age, y = value)) + # 'value' is the SHAP value from melt()
    geom_point(aes(color = value)) + # Coloring points by SHAP value
    scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) + # Color scale
    theme_minimal() +
    labs(title = "SHAP Values for Age", x = "Age", y = "SHAP Value")

##SHAP looping all variables
library(ggplot2)
library(dplyr)

plot_shap_for_feature <- function(feature_name, shap_data, feature_data) {
    # Determine if the feature is categorical or numerical
    feature_type <- if (is.numeric(feature_data[[feature_name]])) "numeric" else "categorical"
    
    # Extract relevant data for plotting
    plot_data <- shap_data %>%
        filter(feature == feature_name) %>%
        mutate(actual_value = feature_data[[feature_name]][observation])
    
    # Generate plot based on feature type
    if (feature_type == "numeric") {
        p <- ggplot(plot_data, aes(x = actual_value, y = value, color = value)) +
            geom_point() +
            scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
            theme_minimal() +
            labs(title = paste("SHAP Values for", feature_name), x = feature_name, y = "SHAP Value")
    } else { # For categorical features
        p <- ggplot(plot_data, aes(x = as.factor(actual_value), y = value, fill = value)) +
            geom_boxplot() +
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = paste("SHAP Values for", feature_name), x = feature_name, y = "SHAP Value")
    }
    
    return(p)
}
# For a numerical feature -- examples
p_numeric_gst_days <- plot_shap_for_feature("gest_age", shap_long, X_prepared)+scale_x_continuous(name = "Gestational Age (weeks)", 
                                                                                                  labels = function(x) ceiling(x / 7),
                                                                                                  breaks = function(x) seq(min(x), max(x), by = 7))
print(p_numeric_gst_days)

# For a numerical feature -- examples
p_numeric_age <- plot_shap_for_feature("age", shap_long, X_prepared)
print(p_numeric_age)

# For a categorical feature
p_categorical_city <- plot_shap_for_feature("city", shap_long, X_prepared)
print(p_categorical_city)

# For a categorical feature
p_categorical_zip <- plot_shap_for_feature("zip", shap_long, X_prepared)
print(p_categorical_zip)

p_categorical_initial_trimester <- plot_shap_for_feature("initial_trimester", shap_long, X_prepared)
print(p_categorical_initial_trimester)

p_categorical_uds_age <- plot_shap_for_feature("uds_age", shap_long, X_prepared)
print(p_categorical_uds_age)


# PDP for the 'age' feature
pdp_age <- model_profile(rf_explainer, variables = "age")
plot(pdp_age)

cp <- predict_profile(rf_explainer, new_observation = ob_data_filtered[1, ])
plot(cp)+ scale_x_continuous(name = "Gestational Age (weeks)", 
                           labels = function(x) ceiling(x / 7),
                           breaks = function(x) seq(min(x), max(x), by = 28))


# Model performance
mp <- model_performance(rf_explainer)
plot(mp)

# Residual diagnostics
rd <- model_diagnostics(rf_explainer)
plot(rd)


# Install and load vip

if (!requireNamespace("vip", quietly = TRUE)) install.packages("vip")
library(vip)

# Assuming 'model' is your regression model
vip(model)


##Trend Analysis ---- 
#Loess and EDA
library(ggplot2)
library(dplyr)

ob_del_trnd <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")
#by City
ob_del_trnd %>%
    ggplot(aes(x = episode_indicator_date_dt, y = birth_wt)) +
    geom_point(aes(color = city), alpha = 0.5) +  # Color-coded by city for additional insights
    geom_smooth(method = "loess", color = "blue", se = FALSE) +  # LOESS curve for trend
    theme_minimal() +
    labs(title = "Birth Weight Over Time",
         x = "Date",
         y = "Birth Weight (g)") +
    theme(legend.position = "bottom")

#by zip
ob_del_trnd %>%
    ggplot(aes(x = episode_indicator_date_dt, y = birth_wt)) +
    geom_point(aes(color = zip), alpha = 0.5) +  # Color-coded by zip for additional insights
    geom_smooth(method = "loess", color = "blue", se = FALSE) +  # LOESS curve for trend
    theme_minimal() +
    labs(title = "Birth Weight Over Time",
         x = "Date",
         y = "Birth Weight (g)") +
    theme(legend.position = "bottom")


##Map ----

library(leaflet)
library(sf)
library(dplyr)

# Assuming 'mdl.ds' contains your data and 'zip_code_sf' is your sf template for the area
zip_code_sf <- readRDS("CSVS service area with Zipcode and Population  with sf and shp.RDS")

# Ensure ZIP codes are in the correct format (first 5 digits only)
mdl.ds.map <- readRDS("mdl.ds.RDS") %>%
    group_by(zip) %>%
    summarise(count = n()) %>%
    head(., -1) 


# Ensure that 'zip' in mdl.ds.map and 'ZCTA5CE20' in zip_code_sf are correctly formatted
mdl.ds.map$zip <- as.character(mdl.ds.map$zip)

# Perform the join, ensuring that the sf object (zip_code_sf) is on the left
mdl.ds.map.joined <- zip_code_sf %>%
    left_join(mdl.ds.map, by = c("ZCTA5CE20" = "zip"))

saveRDS(mdl.ds.map.joined, "OB Data for Leaflet Map.RDS")
# Check the structure of the joined data to confirm it retains its sf properties
#str(mdl.ds.map.joined)

library(leaflet)
library(sf)
library(RColorBrewer)

# Assuming mdl.ds.map.joined is your sf object prepared earlier

# Create a color palette function for dynamic coloring based on 'count'
colorPalette <- colorQuantile(palette = "YlOrRd", domain = mdl.ds.map.joined$count, n = 5)

leaflet(data = mdl.ds.map.joined) %>%
    addTiles() %>%
    addPolygons(fillColor = ~colorPalette(count),
                color = "#BDBDC3",
                fillOpacity = 0.7,
                weight = 1,
                opacity = 1,
                highlight = highlightOptions(weight = 3,
                                             color = "#666",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE),
                label = ~paste("ZIP Code:", ZCTA5CE20, "<br/>Count:", count),
                labelOptions = labelOptions(direction = 'auto', noHide = F, textOnly = TRUE)) %>%
    addLegend(pal = colorPalette, values = ~count, opacity = 0.7, title = "Count",
              position = "bottomright") %>%
    setView(lng = -121.895, lat = 36.674, zoom = 9)  # Adjust the center and zoom level as needed


#Xicor ----

# Ensure necessary libraries are loaded
library(dplyr)
library(purrr)
library(XICOR)

# Prepare your dataset (Assuming mdl.ds.xi is already loaded and ready)

library(dplyr)
mdl.ds.xi <- readRDS("Compact OB Data without NAs and with only Relevant Variables.RDS")

# Transform 'uds_age' to a numeric variable based on its levels
mdl.ds.xi$uds_age_ordinal <- as.numeric(factor(mdl.ds.xi$uds_age))

# Similarly, transform 'initial_trimester' to a numeric variable
mdl.ds.xi$initial_trimester_ordinal <- as.numeric(factor(mdl.ds.xi$initial_trimester))

# Automatically extract top cities and ZIPs based on average birth weight
top_cities <- mdl.ds.xi %>%
    group_by(city) %>%
    summarise(avg_birthweight = mean(birth_wt, na.rm = TRUE)) %>%
    arrange(desc(avg_birthweight)) %>%
    head(10) %>%
    pull(city)

top_zips <- mdl.ds.xi %>%
    group_by(zip) %>%
    summarise(avg_birthweight = mean(birth_wt, na.rm = TRUE)) %>%
    arrange(desc(avg_birthweight)) %>%
    head(10) %>%
    pull(zip)

# Create one-hot encoded variables for the top cities and ZIPs
mdl.ds.xi$city <- factor(mdl.ds.xi$city, levels = top_cities)
mdl.ds.xi$zip <- factor(mdl.ds.xi$zip, levels = top_zips)

# Assuming mdl.ds.xi is your dataset and it's prepared

# Add "UnknownCity" as a new level to the 'city' factor
mdl.ds.xi$city <- factor(mdl.ds.xi$city, levels = c(levels(mdl.ds.xi$city), "UnknownCity"))

# Replace NA values in 'city' with "UnknownCity"
mdl.ds.xi$city[is.na(mdl.ds.xi$city)] <- "UnknownCity"

# Add "UnknownZip" as a new level to the 'zip' factor
mdl.ds.xi$zip <- factor(mdl.ds.xi$zip, levels = c(levels(mdl.ds.xi$zip), "UnknownZip"))

# Replace NA values in 'zip' with "UnknownZip"
mdl.ds.xi$zip[is.na(mdl.ds.xi$zip)] <- "UnknownZip"

# Now, recreate the dummy variables
# Proceed with one-hot encoding
city_dummies <- model.matrix(~ city - 1, data = mdl.ds.xi)
zip_dummies <- model.matrix(~ zip - 1, data = mdl.ds.xi)

# Verify row counts are aligned
print(nrow(mdl.ds.xi))
print(nrow(city_dummies))
print(nrow(zip_dummies))

# Assuming row counts match, combine the datasets
# Combine the original dataset with the one-hot encoded dummy variables
mdl.ds.xi_extended <- cbind(mdl.ds.xi, city_dummies, zip_dummies)


# Define predictors, ensuring to exclude non-numeric variables and include one-hot encoded variables
predictors <- setdiff(names(mdl.ds.xi_extended), c("birth_wt", "city", "zip","gestational_wks", "uds_age", "episode_indicator_date_dt",
                                                   "episode_indicator_code","preg_start_edc_dt", "preg_start_type", "est_delivery_date_dt", "delivery_date_dt", "delivery_doc_date_dt",
                                                   "delivery_doc_code", "spontaneous_abortion_date_dt", "spontaneous_abortion_code",  "initial_trimester", "cityUnknownCity", "zipUnknownZip"))



library(purrr)
library(XICOR)


# Set a seed for reproducibility
set.seed(123)

# Apply xicor across all predictors against birth_wt
xi_cor_results <- map(setNames(nm = predictors), 
                      ~ xicor(mdl.ds.xi_extended[[.x]], mdl.ds.xi_extended$birth_wt))

# Organizing results for easier interpretation

xi_cor_df <- map_df(xi_cor_results, ~ as.data.frame(t(.)), .id = "Variable")

saveRDS(xi_cor_df, "XiCor data.RDS")


# Optional: View the results for all variables
print(xi_cor_df)

#Plot
library(ggplot2)

# Assuming xi_cor_df is structured with 'Variable' and 'V1' columns, where 'V1' holds the xicor values
ggplot(xi_cor_df, aes(x = reorder(Variable, V1), y = V1, fill = V1)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Flips the axes for better readability of variable names
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(x = "Variable", y = "Xi Correlation Coefficient", title = "Xi Correlation with Delivery Birth Weight") +
    theme_minimal()

