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

ob_data_cln_1 <- readRDS("OB Data, Raw.RDS")

# ob_data_cln_1 %>%  DataExplorer::introduce()
# 
# ob_data_cln_1 %>% explore()

hist(ob_data_cln_1$age)

range(ob_data_cln_1$age)

summary(ob_data_cln_1$age)

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
ob_data_clean_1 <- ob_data_slim %>%
    filter(!is.na(delivery_birthweight_num2))

# First, attempt to convert delivery_birthweight_num2 to numeric to identify NAs
ob_data_clean_1$delivery_birthweight_num2 <- as.numeric(gsub("[^0-9.]", "", ob_data_clean_1$delivery_birthweight_num2))

# Correct city name variations in the cleaned data
ob_data_clean_2 <- ob_data_clean_1 %>%
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

mdl.ds <- readRDS("OB DATA for Modelling on Birth Weight.RDS")

##LMN ----
# Fit the multiple linear regression model
model <- lm(delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester, data = mdl.ds)
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
plot(gbm_model, n.plots = 3, write.title = FALSE, plot.layout = c(3, 1))


##XGboost ----
# Load the required library
library(xgboost)

# Convert initial_trimester to numeric
mdl_gbst_filtered$initial_trimester <- as.numeric(as.character(mdl_gbst_filtered$initial_trimester))

# Define the model formula
formula <- delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester

# Create a matrix for the predictors
X <- model.matrix(formula, data = mdl_gbst_filtered)

# Extract the response variable
y <- mdl_gbst_filtered$delivery_birthweight_num2

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



## Other Analysis ----
# Load the packages
library(DALEX)
library(pdp)
library(iml)
library(randomForest)
library(ggplot2)

# Example model (assuming your data is in 'ob_data_filtered')
model <- lm(delivery_birthweight_num2 ~ age + gestational_days + city + zip + initial_trimester, data = ob_data_filtered)

# Create an explainer for the model
explainer <- explain(model, data = ob_data_filtered[, c("age", "gestational_days", "city", "zip", "initial_trimester")], y = ob_data_filtered$delivery_birthweight_num2)

# Variable importance
vi <- model_parts(explainer, type = "variable_importance")
plot(vi)

# Prediction breakdown for the first observation
pb <- predict_parts(explainer, ob_data_filtered[1, ], type = "break_down")
plot(pb)

# Partial dependence plot for 'age'
pdp_age <- partial(model, pred.var = "age", train = ob_data_filtered, plot = TRUE, plot.engine = "ggplot")

# Assuming 'model' is your trained model# Ensure 'model' and 'ob_data_filtered' are correctly defined as before
# Assuming 'model' is your trained model and 'ob_data_filtered' is your dataset

# Define the feature matrix X excluding the target variable
# X <- ob_data_filtered[, c("age", "gestational_days", "city", "zip", "initial_trimester")]
# 
# # Define the target variable y
# y <- ob_data_filtered$delivery_birthweight_num2
# 
# # Create the Predictor object with correctly specified X and y
# predictor <- Predictor$new(model = model, data = X, y = y)
# 
# # Attempt to create FeatureEffects object again
# featureEffects <- FeatureEffects$new(predictor)
# 
# plot(featureEffects)
# 
# # Shapley values for the first observation
# shapley <- Shapley$new(predictor, x.interest = ob_data_filtered[1, ])
# plot(shapley)

# Assuming a linear model for simplicity
coefficients <- coef(model)[-1]  # Excluding the intercept
feature_names <- names(coefficients)
importances <- abs(coefficients)  # Absolute value for importance

# Create a data frame for plotting
importance_df <- data.frame(Feature = feature_names, Importance = importances)

# Plot using ggplot2
library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    xlab("Features") +
    ylab("Importance (Absolute Coefficient Value)") +
    ggtitle("Feature Importance")


# Install and load vip
if (!requireNamespace("vip", quietly = TRUE)) install.packages("vip")
library(vip)

# Assuming 'model' is your regression model
vip(model)


## Deep Neural Network ----
# Subsetting the dataset to include only the relevant variables
ob_data_filtered_relevant <- ob_data_filtered %>%
    select(delivery_birthweight_num2, age, gestational_days, city, zip, initial_trimester)

# Removing rows with NA values
ob_data_filtered_complete <- na.omit(ob_data_filtered_relevant)

# Splitting the cleaned dataset again
set.seed(42)
indices <- sample(1:nrow(ob_data_filtered_complete), size = 0.8 * nrow(ob_data_filtered_complete))
train_data <- ob_data_filtered_complete[indices, ]
test_data <- ob_data_filtered_complete[-indices, ]

# Since city and zip are categorical, let's ensure they're factored and then create dummy variables
train_data <- train_data %>%
    mutate(across(c(city, zip), as.factor)) %>%
    mutate(across(c(age, gestational_days, initial_trimester), scale))

test_data <- test_data %>%
    mutate(across(c(city, zip), as.factor)) %>%
    mutate(across(c(age, gestational_days, initial_trimester), scale))

# Convert categorical variables to dummy variables
train_data <- model.matrix(~.-1, data = train_data)
test_data <- model.matrix(~.-1, data = test_data)

# Preparing the final input and output matrices for the model
x_train <- as.matrix(train_data[, -which(colnames(train_data) == "delivery_birthweight_num2")])
y_train <- train_data[, "delivery_birthweight_num2"]

x_test <- as.matrix(test_data[, -which(colnames(test_data) == "delivery_birthweight_num2")])
y_test <- test_data[, "delivery_birthweight_num2"]


# Model architecture
model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = 'relu', input_shape = dim(x_train)[2]) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 1)

# Compile the model
model %>% compile(
    optimizer = 'rmsprop',
    loss = 'mse',
    metrics = 'mean_absolute_error'
)

# Train the model
history <- model %>% fit(
    x_train,
    y_train,
    epochs = 100,
    validation_split = 0.2
)


model %>% evaluate(x_test, y_test)

predictions <- model %>% predict(x_test)

# Compute MSE and MAE
mse <- mean((predictions - y_test)^2)
mae <- mean(abs(predictions - y_test))

# Print the errors
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

# For R², you might need to calculate manually or use another package
r_squared <- 1 - sum((predictions - y_test)^2) / sum((y_test - mean(y_test))^2)
cat("R-squared:", r_squared, "\n")


library(ggplot2)

data_to_plot <- data.frame(Actual = y_test, Predicted = as.vector(predictions))
ggplot(data_to_plot, aes(x = Actual, y = Predicted)) +
    geom_point(color = 'blue') +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = "Actual vs. Predicted", x = "Actual Birthweight", y = "Predicted Birthweight") +
    theme_minimal()


data_to_plot$Residuals <- data_to_plot$Actual - data_to_plot$Predicted
ggplot(data_to_plot, aes(x = Actual, y = Residuals)) +
    geom_point(color = 'blue') +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Residuals vs. Actual", x = "Actual Birthweight", y = "Residuals") +
    theme_minimal()

