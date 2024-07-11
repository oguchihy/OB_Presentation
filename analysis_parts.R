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


#1. Mapping----

leaflet_map = source(bestLfletMap.R, local = FALSE, echo = FALSE)


#2. Shiny "in-line" from q_sh_doctmplt.qmd----

in_line_shiny = source(q_sh_doctmplt.qmd, local = FALSE, echo = FALSE)

#3.Generic Stats ----

parts_stats_block <- readRDS("Complete Ready OB Dataset for Analytics.RDS")

ob_hghRsk <- parts_stats_block
table(ob_hghRsk$hghRsk)


dte_range_NMC_data <- range(parts_stats_block$adm_date)
del_tm_dur_range <- round(range((parts_stats_block$gest_age_days)/7),1)
age_range <- range(parts_stats_block$age)
nbr_del_nmc <- nrow(parts_stats_block)
prcnt_preterm <- round(sum(parts_stats_block$gest_age_days <259) / sum(parts_stats_block$gest_age_days >=140)*100,0)
gest_age_range <- range(parts_stats_block$gest_age_days, na.rm = TRUE)
prcnt_hghRsk <- round(sum(parts_stats_block$hghRsk=="yes")/nrow(parts_stats_block)*100,0)

# 4. Models.
#XiCor:

# Install and load necessary packages
install.packages("xicor")
install.packages("ggplot2")

library(xicor)
library(dplyr)
library(ggplot2)

# Load your dataset and prepare the data
ob_data_clst_xi <- readRDS("Complete Ready OB Dataset for Analytics.RDS") %>%
  dplyr::select(zip, age, grav, para, diff_grav_para, lbr_type_cnsldt, membrane_rupture, intrapartal_conditions, 
                presentation_cnsldt, conditions_cnsldt, uds_age, adm_to_del_tm_cat, hghRsk, cluster) %>%
  mutate(cluster = if_else(cluster == 2, 1, 0))

# Ensure all predictors are numeric
ob_data_clst_xi <- ob_data_clst_xi %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(across(where(is.numeric), as.numeric))

# List of predictor variables
predictor_vars <- colnames(ob_data_clst_xi)[!colnames(ob_data_clst_xi) %in% c("cluster")]

# List of predictor variables
predictor_vars <- colnames(ob_data_clst_xi)[!colnames(ob_data_clst_xi) %in% c("cluster")]

# Calculate xi correlations
xi_cor_results <- sapply(predictor_vars, function(var) {
  xicor(ob_data_clst_xi[[var]], ob_data_clst_xi$cluster)
})

# Create a data frame to store the results
xi_cor_results_df <- data.frame(
  predictor = predictor_vars,
  xi_cor = xi_cor_results
)

# Print the results
print(xi_cor_results_df)

# Create the bar plot
ggplot(xi_cor_results_df, aes(x = reorder(predictor, xi_cor), y = xi_cor)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Xi Correlation with Cluster Outcome",
       x = "Predictor Variables",
       y = "Xi Correlation") +
  theme_minimal()

#Discover numerical coding of categoricals:
# Install and load necessary packages
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(dplyr)

# Load your dataset
ob_data_clst_xi <- readRDS("Complete Ready OB Dataset for Analytics.RDS") %>%
  dplyr::select(zip, age, grav, para, diff_grav_para, lbr_type_cnsldt, membrane_rupture, intrapartal_conditions, 
                presentation_cnsldt, conditions_cnsldt, uds_age, adm_to_del_tm_cat, hghRsk, cluster)

# Function to get the mapping of original to numeric values
get_numeric_mapping <- function(df, column) {
  factor_column <- as.factor(df[[column]])
  levels_mapping <- levels(factor_column)
  numeric_values <- as.numeric(factor_column)
  unique_values <- unique(factor_column)
  mapping <- data.frame(Original = unique_values, Numeric = numeric_values[match(unique_values, factor_column)])
  return(mapping)
}

# Apply the function to all factor variables and create a list of mappings
factor_vars <- names(ob_data_clst_xi)[sapply(ob_data_clst_xi, is.factor)]
mappings <- lapply(factor_vars, function(col) {
  mapping_df <- get_numeric_mapping(ob_data_clst_xi, col)
  mapping_df <- mapping_df %>% arrange(Numeric)  # Sort by Numeric values
  mapping_df$Variable <- col
  return(mapping_df)
})

# Combine all mappings into a single data frame
combined_mapping_df <- do.call(rbind, mappings)

# Print the combined mapping
print(combined_mapping_df)

# Apply the conversion to the entire dataset
ob_data_clst_xi <- ob_data_clst_xi %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.))))

# Print the first few rows of the converted dataset
print(head(ob_data_clst_xi))

# Logisitic, broom

# Install and load necessary packages
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(dplyr)

# Load your dataset and prepare the data
ob_data_clst_xi <- readRDS("Complete Ready OB Dataset for Analytics.RDS") %>%
  dplyr::select(zip, age, grav, para, diff_grav_para, lbr_type_cnsldt, membrane_rupture, intrapartal_conditions, 
                presentation_cnsldt, conditions_cnsldt, uds_age, adm_to_del_tm_cat, hghRsk, cluster) %>%
  mutate(cluster = ifelse(cluster == 2, 1, 0))

# Function to get the mapping of original to numeric values
get_numeric_mapping <- function(df, column) {
  factor_column <- as.factor(df[[column]])
  numeric_values <- as.numeric(factor_column)
  unique_values <- unique(factor_column)
  mapping <- data.frame(Original = unique_values, Numeric = numeric_values[match(unique_values, factor_column)])
  return(mapping)
}

# Apply the function to all factor variables and create a list of mappings
factor_vars <- names(ob_data_clst_xi)[sapply(ob_data_clst_xi, is.factor)]
mappings <- lapply(factor_vars, function(col) {
  if (length(unique(ob_data_clst_xi[[col]])) > 1) {  # Ensure the column has more than one unique value
    mapping_df <- get_numeric_mapping(ob_data_clst_xi, col)
    mapping_df <- mapping_df %>% arrange(Numeric)  # Sort by Numeric values
    mapping_df$Variable <- col
    return(mapping_df)
  } else {
    return(NULL)  # Return NULL for columns with only one unique value
  }
})

# Remove NULL elements from the list
mappings <- Filter(Negate(is.null), mappings)

# Combine all mappings into a single data frame
combined_mapping_df <- do.call(rbind, mappings)

# Print the combined mapping
print(combined_mapping_df)

# Apply the conversion to the entire dataset
ob_data_clst_xi <- ob_data_clst_xi %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.))))

# Print the first few rows of the converted dataset
print(head(ob_data_clst_xi))


# Create summary tables for each factor variable
summary_tables <- lapply(factor_vars, function(col) {
  summary_table <- get_numeric_mapping(ob_data_clst_xi, col)
  summary_table <- summary_table %>% arrange(Numeric)  # Sort by Numeric values
  return(summary_table)
})

# Print the summary tables
lapply(summary_tables, print)




# Install and load necessary packages
if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")

library(broom)
library(ggplot2)

# Fit a logistic regression model
model <- glm(cluster ~ ., data = ob_data_clst_xi, family = binomial)

# Tidy the model output
model_tidy <- tidy(model)

# Plot the coefficients
ggplot(model_tidy, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Logistic Regression Coefficients",
       x = "Predictor Variables",
       y = "Coefficient Estimate")


#Working XICOR pair shiny:

---
  title: "Interactive Xi Correlation Analysis"
format: html
server: shiny
---
  
  ```{r setup, include=FALSE}
library(shiny)
library(XICOR)
library(dplyr)
library(ggplot2)
library(DT)

# Assuming the dataset is loaded here
ob_data_clst_xi <- readRDS("Complete Ready OB Dataset for Analytics.RDS") %>%
  dplyr::select(zip, lbr_type_cnsldt, membrane_rupture, intrapartal_conditions,
                presentation_cnsldt, conditions_cnsldt, uds_age, adm_to_del_tm_cat, hghRsk, cluster,
                age, grav, para, diff_grav_para) %>%
  mutate(across(c(zip, lbr_type_cnsldt, membrane_rupture, intrapartal_conditions,
                  presentation_cnsldt, conditions_cnsldt, uds_age, adm_to_del_tm_cat, hghRsk, cluster), as.factor)) %>%
  mutate(across(c(zip, lbr_type_cnsldt, membrane_rupture, intrapartal_conditions,
                  presentation_cnsldt, conditions_cnsldt, uds_age, adm_to_del_tm_cat, hghRsk, cluster), as.numeric))
```


```{r}
#| context: server
output$xiCorPlot <- renderPlot({
  req(input$var1, input$var2)
  if (input$var1 != input$var2) {
    xi_value <- xicor(ob_data_clst_xi[[input$var1]], ob_data_clst_xi[[input$var2]])
    xi_data <- data.frame(Variable1 = input$var1, Variable2 = input$var2, Xi = xi_value)
    
    ggplot(xi_data, aes(x = Variable1, y = Xi, fill = Variable2)) +
      geom_col() +
      labs(title = paste("Xi Correlation between", input$var1, "and", input$var2),
           x = "Variable Pair", y = "Xi Correlation") +
      theme_minimal()
  } else {
    ggplot() + labs(title = "Please select two different variables")
  }
})

output$xiCorTable <- DT::renderDT({
  req(input$var1, input$var2)
  if (input$var1 != input$var2) {
    xi_value <- xicor(ob_data_clst_xi[[input$var1]], ob_data_clst_xi[[input$var2]])
    datatable(data.frame(Variable1 = input$var1, Variable2 = input$var2, Xi = xi_value), options = list(pageLength = 5, scrollX = TRUE))
  } else {
    datatable(data.frame(Message = "Select different variables"), options = list(pageLength = 5, scrollX = TRUE))
  }
})

```

```{r}
# Define the UI layout
fluidPage(
  titlePanel("Interactive Xi Correlation Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Select the first variable:", choices = names(ob_data_clst_xi)),
      selectInput("var2", "Select the second variable:", choices = names(ob_data_clst_xi))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Xi Correlation Plot", plotOutput("xiCorPlot")),
        tabPanel("Xi Correlation Table", DTOutput("xiCorTable"))
      )
    )
  )
)

```

##########