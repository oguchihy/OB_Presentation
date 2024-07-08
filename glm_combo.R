#Design binary outcomes for GLM.
ob_data_fctr$binary_outcome <- as.factor(ob_data_fctr$cluster)


ob_data_fctr.bin <- ob_data_fctr %>% 
  mutate(bin_del_CS = as.factor(if_else(del_method_cnsldt =="C/S, Primary", 1, 0)),
         bin_lbr_non_spont = as.factor(if_else(lbr_type_cnsldt =="Spontaneous", 0,1)),
         bin_conditions = as.factor(if_else(conditions_cnsldt == "None", 0,1)),
         bin_events = as.factor(if_else(intrapartal_events == "None", 0, 1)))

suppressWarnings({
  model_del <- glm(bin_del_CS ~ zip_category + age + grav + para + gender + uds_age + hghRsk  + lbr_type_cnsldt, 
                   data = ob_data_fctr.bin, family = binomial())
  
  model_lbr <- glm(bin_lbr_non_spont ~ zip_category + age + grav + para + gender + uds_age + hghRsk + del_method_cnsldt, 
                   data = ob_data_fctr.bin, family = binomial())
  
  model_conditions <- glm(bin_conditions ~ zip_category + age + grav + para + gender + uds_age + hghRsk + lbr_type_cnsldt + del_method_cnsldt, 
                          data = ob_data_fctr.bin, family = binomial())
  
  model_events <- glm(bin_events ~ zip_category + age + grav + para + gender + uds_age + hghRsk + lbr_type_cnsldt + del_method_cnsldt, 
                      data = ob_data_fctr.bin, family = binomial())
})

library(dplyr)
library(ggplot2)
library(purrr)

# Function to extract results and plot
extract_and_plot <- function(model, title, reference_levels) {
  # Extract coefficients, confidence intervals, and p-values
  coef_summary <- summary(model)$coefficients
  
  # Handle potential errors in confidence interval calculation
  conf_int <- tryCatch(
    confint(model),
    error = function(e) {
      warning("Error in calculating confidence intervals: ", conditionMessage(e))
      return(matrix(NA, nrow = nrow(coef_summary), ncol = 2))
    }
  )
  
  # Combine results into a single data frame
  results <- as.data.frame(coef_summary)
  results$conf.low <- conf_int[, 1]
  results$conf.high <- conf_int[, 2]
  
  # Add predictor names
  results$predictor <- rownames(results)
  rownames(results) <- NULL
  
  # Convert log-odds to odds ratios
  results <- results %>%
    mutate(OR = exp(Estimate),
           conf.low = exp(conf.low),
           conf.high = exp(conf.high),
           p.value = `Pr(>|z|)`)
  
  # Remove rows with missing values
  results <- results[complete.cases(results), ]
  
  # Create the plot
  plot <- ggplot(results, aes(x = reorder(predictor, OR), y = OR)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    geom_text(aes(label = sprintf("p = %.3f", p.value), color = ifelse(p.value < 0.05, "red", "black")), 
              hjust = -0.3, size = 3) +
    geom_text(aes(label = sprintf("OR = %.2f", OR)), vjust = -1.5, size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +  # Add a line at OR = 1 for reference
    scale_y_log10(limits = c(0.5, 10)) +  # Start y-axis from 0.5
    labs(title = title,
         x = "Predictor",
         y = "Odds Ratio (log scale)",
         caption = reference_levels) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "none",
          plot.caption = element_text(size = 10, hjust = 1, vjust = 1, face = "italic")) +
    coord_flip()  # Flip coordinates for better readability
  
  return(plot)
}


# List of models and titles
models <- list(
  model_del = model_del,
  model_lbr = model_lbr,
  model_conditions = model_conditions,
  model_events = model_events
)

titles <- c(
  "Odds Ratios for Delivery Method (C/S, Primary)",
  "Odds Ratios for Labor Type (Non-Spontaneous)",
  "Odds Ratios for Conditions",
  "Odds Ratios for Events"
)

# Reference levels for each model
reference_levels_list <- c(
  "Reference Levels:\nbin_del_CS = 0 (No Primary C/S)",
  "Reference Levels:\nbin_lbr_non_spont = 0 (Spontaneous Delivery)",
  "Reference Levels:\nbin_conditions = 0 (None Intrapartal Conditions)",
  "Reference Levels:\nbin_events = 0 (None Intrapartal Events)"
)

# Apply the function to each model
plots <- map2(models, titles, ~ extract_and_plot(.x, .y, reference_levels_list[[which(titles == .y)]]))

# Print plots
plots


