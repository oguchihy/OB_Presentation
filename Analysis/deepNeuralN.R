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

