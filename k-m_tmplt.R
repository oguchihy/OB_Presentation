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
library(leaflet)
library(sf)
library(tableone)
library(DT)
library(kableExtra)

library(survminer)
library(survival)
library(ggplot2)

# Filter out records with non-positive time values
ob_data_srvl <- readRDS("All Factored Complete Ready OB Dataset for Analytics.RDS") %>%
  filter(time >= 0)%>% 
  mutate(cluster = as.factor(if_else(cluster == 2,1,0)))

# Identify the top 5 ZIP codes based on frequency
top_zip_codes <- ob_data_srvl %>%
  count(zip) %>%
  top_n(5, wt = n) %>%
  pull(zip)

print(top_zip_codes)

# Filter the data to include only the top 5 ZIP codes
filtered_data <- ob_data_srvl %>%
  filter(zip %in% top_zip_codes)


# Fit Kaplan-Meier survival curves
km_fit <- survfit(Surv(time, event) ~ cluster, data = filtered_data)

# Plot using ggsurvplot
ggsurvplot(
  km_fit,
  data = filtered_data,
  pval = TRUE,
  risk.table = "abs_pct",
  ggtheme = theme_minimal(),
  legend.title = "Cluster",
  legend.labs = levels(filtered_data$cluster)
)


# Plot using ggsurvplot
ggsurvplot(
  km_fit,
  data = filtered_data,
  fun = "cumhaz",
  pval = TRUE,
  risk.table = "abs_pct",
  ggtheme = theme_minimal(),
  legend.title = "Cluster",
  legend.labs = levels(filtered_data$cluster)
)


# Plot using ggsurvplot
ggsurvplot(
  km_fit,
  data = filtered_data,
  fun = "event",
  pval = TRUE,
  risk.table = "abs_pct",
  ggtheme = theme_minimal(),
  legend.title = "Cluster",
  legend.labs = levels(filtered_data$cluster)
)
