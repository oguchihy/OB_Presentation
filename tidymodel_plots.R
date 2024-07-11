library(ggplot2)
# Assuming 'tidy_model' is your tidied model output
p <- ggplot(tidy_model, aes(x = term, y = estimate, fill = estimate)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ y.level, scales = "free_x") +  # Adjusted this line
    theme_minimal() +
    labs(title = "Multinomial Logistic Regression Coefficients",
         x = "Predictor Variables", y = "Estimates") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)


library(ggpubr)
ggarrange(plotlist = list(p), labels = "AUTO")


library(plotly)
p <- ggplot(tidy_model, aes(x = term, y = estimate, color = estimate)) +
    #geom_point() +
    geom_bar(stat = "identity") +
    facet_wrap(~ y.level)
# Convert ggplot to plotly
plotly::ggplotly(p)

#Heatmap
# Install Bioconductor installer if you haven't already
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Install ComplexHeatmap using BiocManager
BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)
ht_opt$message = FALSE
library(circlize)
ht_opt$message = FALSE
library(magic)
ht_opt$message = FALSE
# Assuming plot_data is properly prepared with rows as terms and columns as conditions
Heatmap(as.matrix(plot_data), 
        name = "Estimate", 
        column_title = "Predictor Importance", 
        row_title = "Terms",
        cluster_rows = FALSE,
        cluster_columns = TRUE)

#cowplot or patchwork

# library(cowplot)
# library(patchwork)
# # Assume 'p1', 'p2' are different ggplot objects
# p_combined <- p1 / p2
# plot(p_combined)

library(rayshader)
tidy_model %>%
    ggplot(aes(x = term, y = condition, fill = estimate)) +
    geom_tile() -> p
p %>% plot_gg(height = 2, width = 5, multicore = TRUE, scale = 250)


library(ggplot2)

# Plot showing estimates for each predictor across different conditions
p <- ggplot(tidy_model, aes(x = term, y = estimate, fill = term)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~ y.level, scales = "free_y") +
    theme_minimal() +
    labs(title = "Coefficient Estimates by Term across Conditions",
         x = "Predictor Term", y = "Coefficient Estimate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)

# Now, convert this plot into a 3D plot using rayshader
library(rayshader)
library(tidyr)
library(dplyr)
library(tibble)

# Pivot data without converting to row names
coeff_matrix_df <- tidy_model %>%
    pivot_wider(names_from = y.level, values_from = estimate) %>%
    select(-c(std.error, statistic, p.value))

# Make row names unique by combining term with levels (if needed)
coeff_matrix_df <- coeff_matrix_df %>%
    mutate(unique_term = paste(term, names(coeff_matrix_df)[-1], sep = "_")) %>%
    select(-term) %>%
    column_to_rownames(var = "unique_term")

# Convert to matrix
coeff_matrix <- as.matrix(coeff_matrix_df)

# Check the matrix
print(coeff_matrix)

library(ggplot2)
library(rayshader)

# Assuming coeff_matrix is now a proper matrix without duplicate row names
library(ggplot2)

library(ggplot2)

# Assuming 'coeff_matrix' is your data prepared for plotting
# and 'reshape2::melt()' has been applied successfully
melted_data <- reshape2::melt(coeff_matrix)

library(ggplot2)

# Assuming 'melted_data' is your reshaped data ready for plotting
tile_plot <- ggplot(melted_data, aes(y = Var1, x = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(title = "Visualization of Coefficients", x = "Value", y = "Predictor Term and Condition") +
    theme(axis.text.x = element_text(angle = 45))  # Adjust x-axis text orientation as needed

# Display the plot
print(tile_plot)


# Display the plot
print(tile_plot)



# Render with rayshader
tile_plot %>%
    plot_gg(height = 5, width = 5, scale = 250)
