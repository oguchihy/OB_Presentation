# Ensure necessary libraries are loaded
library(dplyr)
library(purrr)
library(XICOR)

# Prepare your dataset (Assuming mdl.ds.xi is already loaded and ready)

library(dplyr)
mdl.ds.xi <- mdl.ds %>%
    filter(!is.na(delivery_birthweight_num2) & !is.na(age) & !is.na(gestational_days) & 
               !is.na(city) & !is.na(zip) & !is.na(initial_trimester) & !is.na(uds_age))


# Transform 'uds_age' to a numeric variable based on its levels
mdl.ds.xi$uds_age_ordinal <- as.numeric(factor(mdl.ds.xi$uds_age))

# Similarly, transform 'initial_trimester' to a numeric variable
mdl.ds.xi$initial_trimester_ordinal <- as.numeric(factor(mdl.ds.xi$initial_trimester))

# Automatically extract top cities and ZIPs based on average birth weight
top_cities <- mdl.ds.xi %>%
    group_by(city) %>%
    summarise(avg_birthweight = mean(delivery_birthweight_num2, na.rm = TRUE)) %>%
    arrange(desc(avg_birthweight)) %>%
    head(10) %>%
    pull(city)

top_zips <- mdl.ds.xi %>%
    group_by(zip) %>%
    summarise(avg_birthweight = mean(delivery_birthweight_num2, na.rm = TRUE)) %>%
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
predictors <- setdiff(names(mdl.ds.xi_extended), c("delivery_birthweight_num2", "city", "zip","gestational_wks", "uds_age", "episode_indicator_date_dt",
                            "episode_indicator_code","preg_start_edc_dt", "preg_start_type", "est_delivery_date_dt", "delivery_date_dt", "delivery_doc_date_dt",
                            "delivery_doc_code", "spontaneous_abortion_date_dt", "spontaneous_abortion_code",  "initial_trimester", "cityUnknownCity", "zipUnknownZip"))

    
    
library(purrr)
library(XICOR)


# Set a seed for reproducibility
set.seed(123)

# Apply xicor across all predictors against delivery_birthweight_num2
xi_cor_results <- map(setNames(nm = predictors), 
                      ~ xicor(mdl.ds.xi_extended[[.x]], mdl.ds.xi_extended$delivery_birthweight_num2))

# Organizing results for easier interpretation
xi_cor_df <- map_df(xi_cor_results, ~ as.data.frame(t(.)), .id = "Variable")

# Optional: View the results for all variables
print(xi_cor_df)
