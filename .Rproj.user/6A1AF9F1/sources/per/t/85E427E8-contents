# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(kableExtra)

setwd(getwd())
# Load the new data from the Excel file
file_path <- "sample_dataset_Q2_2024_corrected_final.xlsx"
new_data <- readxl::read_xlsx("sample_dataset_Q2_2024_corrected.xlsx")

# Summarize the new data to weekly values
new_data <- new_data %>%
  mutate(Date = as.Date(Date),
         week_start = floor_date(Date, "week")) %>%
  group_by(week_start) %>%
  summarise(new_deliveries = sum(Value, na.rm = TRUE)) %>%
  ungroup()

# Load and summarize deliveries by week for the original data
ob_data_ts <- readRDS("Working OB Dataset.RDS") %>%
  mutate(delivery_date = as.Date(delivery_date),
         week_start = floor_date(delivery_date, "week")) %>%
  group_by(week_start) %>%
  summarise(deliveries = sum(event, na.rm = TRUE)) %>%
  ungroup()

# Ensure date columns are Date objects
ob_data_ts$week_start <- as.Date(ob_data_ts$week_start)

# Prepare data for prophet
prophet_data <- ob_data_ts %>%
  rename(ds = week_start, y = deliveries)

# Fit prophet model with weekly seasonality
prophet_model <- prophet(prophet_data, weekly.seasonality = TRUE)

# Generate future dataframe
future <- make_future_dataframe(prophet_model, periods = 13, freq = "week")

# Forecast
forecast <- predict(prophet_model, future)

# Ensure forecast dataframe ds column is Date object
forecast$ds <- as.Date(forecast$ds)

# Combine historical data and forecast
combined_data <- bind_rows(
  prophet_data %>% mutate(type = "Historical"),
  forecast %>% mutate(type = "Forecast", y = yhat) %>% dplyr::select(ds, y, type)
)

# Check for and remove duplicate rows if any
combined_data <- combined_data %>%
  distinct(ds, .keep_all = TRUE)

# Round forecast values
forecast <- forecast %>% 
  mutate(yhat = round(yhat))

# Extract forecast values as DataFrame
forecast_df <- combined_data %>%
  filter(type == "Forecast") %>%
  dplyr::select(ds, y) %>%
  rename(Date = ds, Forecast = y)

# Merge forecast data with new data
merged_data <- left_join(new_data, forecast_df, by = c("week_start" = "Date"))

# Ensure there are no NA values in forecast data
merged_data <- merged_data %>% filter(!is.na(Forecast))

# Plot the forecast and new data
p_ts_frcst <- ggplot2::ggplot() +
  geom_line(data = combined_data, aes(x = ds, y = y, color = type), size = 1) +
  geom_line(data = merged_data, aes(x = week_start, y = new_deliveries, color = "New Data"), size = 1) +
  scale_x_date(date_breaks = "1 week", date_labels = "%Y-%U") +
  labs(title = "Forecast vs New Data",
       x = "Week",
       y = "Number of Deliveries",
       color = "Legend") +
  scale_color_manual(values = c("Historical" = "black", "Forecast" = "orange", "New Data" = "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display the plot
ggplotly(p_ts_frcst)

# Tabulate the forecast and new data side by side
merged_data %>%
  dplyr::select(week_start, new_deliveries, Forecast) %>%
  kbl() %>%
  kable_styling(bootstrap_options = "striped", full_width = F)