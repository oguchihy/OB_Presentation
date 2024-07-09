library(XICOR)
library(dplyr)
library(ggplot2)

# Assuming ob_data_clst_xi is already loaded with your data
ob_data_clst_xi <- readRDS("Complete Ready OB Dataset for Analytics.RDS") %>%
  dplyr::select(zip, lbr_type_cnsldt, membrane_rupture, intrapartal_conditions,
                presentation_cnsldt, conditions_cnsldt, uds_age, adm_to_del_tm_cat, hghRsk, cluster,
                age, grav, para, diff_grav_para) %>%
  mutate(across(c(zip, lbr_type_cnsldt, membrane_rupture, intrapartal_conditions,
                  presentation_cnsldt, conditions_cnsldt, uds_age, adm_to_del_tm_cat, hghRsk, cluster), as.factor))

# Create a list of all categorical variables
categorical_vars <- c("zip", "lbr_type_cnsldt", "membrane_rupture", "intrapartal_conditions",
                      "presentation_cnsldt", "conditions_cnsldt", "uds_age", "adm_to_del_tm_cat", "hghRsk", "cluster")

# Calculate Xi correlation for all pairs of categorical variables
results <- expand.grid(Var1 = categorical_vars, Var2 = categorical_vars, stringsAsFactors = FALSE)
results <- results[results$Var1 != results$Var2,]  # Remove self-pairs

results$Xi <- mapply(function(x, y) xicor(as.numeric(ob_data_clst_xi[[x]]), as.numeric(ob_data_clst_xi[[y]])), 
                     results$Var1, results$Var2)

# View results
print(results)

# Plotting all results
ggplot(results, aes(x = Var1, y = Xi, fill = Var2)) +
  geom_col(position = position_dodge()) +
  labs(title = "Xi Correlations Among Categorical Variables", x = "Variable Pairs", y = "Xi Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




