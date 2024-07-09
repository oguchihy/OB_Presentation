# Cluster Analysis
set.seed(123)

# Perform K-means clustering
k <- 3
kmeans_result <- kmeans(mdl_clustering_scaled, centers = k, nstart = 25)

# Add the cluster assignments to the original dataset
ob_data_compact$cluster <- as.factor(kmeans_result$cluster)

# Map numeric cluster IDs to meaningful names
cluster_names <- c("full term", "preterm", "early term")
names(cluster_names) <- 1:3
ob_data_compact$cluster <- factor(ob_data_compact$cluster, levels = 1:3, labels = cluster_names)

# Now, summarizing clusters
cluster_summary <- ob_data_compact %>%
    group_by(cluster) %>%
    summarise(across(c(age, gest_age_days, weight), mean, na.rm = TRUE))

print(cluster_summary)

# Plotting
library(ggplot2)

ggplot(ob_data_compact, aes(x = gest_age_days, y = weight, color = cluster)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = c(3000, 4000), linetype = "dashed", color = "red") +
    geom_vline(xintercept = 259, linetype = "dashed", color = "blue") +
    theme_minimal() +
    scale_x_continuous(
        name = "Gestational Age in completed Weeks",  # Rename the x-axis to "Weeks"
        breaks = seq(0, max(ob_data_compact$gest_age_days), by = 7),  # Set breaks every 7 days
        labels = function(x) floor(x / 7)  # Convert days to floor weeks
    ) +
    labs(title = "Cluster Analysis on Gestational Age and Birth Weights",
         x = "Gestational Age", y = "Delivery Birthweight")
