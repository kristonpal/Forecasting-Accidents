# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)
library(caret)

# Loading the dataset
accidents <- read.csv("US_Accidents_March23_sampled_500k.csv")

# Filtering data for Connecticut
accidents_ct <- accidents %>% filter(State == "CT")

# Creating a datetime column
accidents_ct$Start_Time <- as.POSIXct(accidents_ct$Start_Time, format="%Y-%m-%d %H:%M:%S")

# Extracting the relevant time-based features
accidents_ct$month <- month(accidents_ct$Start_Time)
accidents_ct$year <- year(accidents_ct$Start_Time)

# Calculating accidents occurred monthly
time_series_ct <- accidents_ct %>%
  group_by(year, month) %>%
  summarise(accident_count = n())

# Insights into Seasonal Trends
ggplot(time_series_ct, aes(x = as.Date(paste0(year, "-", month, "-01")), y = accident_count)) +
  geom_line() +
  labs(title = "Accident Counts Over Time (Connecticut)",
       x = "Date",
       y = "Accident Count") +
  theme_minimal()

# Converting to time series data
ts_data <- ts(time_series_ct$accident_count, frequency = 12)  # Assuming monthly frequency

# Splitting data into training and testing sets
train_size <- floor(0.8 * length(ts_data))
ts_train_data <- head(ts_data, train_size)
ts_test_data <- tail(ts_data, length(ts_data) - train_size)

# Building Linear Regression Model
linear_reg_model <- tslm(ts_train_data ~ trend + season)

# Predicting with Linear Regression
linear_reg_preds_ts <- forecast(linear_reg_model, h = length(ts_test_data))

# Calculating Mean Absolute Error for Linear Regression
linear_reg_mae_ts <- mean(abs(ts_test_data - linear_reg_preds_ts$mean))

# Building the ARIMA Model
arima_model <- arima(ts_train_data, order = c(4, 1, 2))

# Predicting with ARIMA
arima_forecast <- forecast(arima_model, h = length(ts_test_data))
arima_preds_ts <- as.numeric(arima_forecast$mean)

# Calculating Mean Absolute Error for ARIMA
arima_mae_ts <- mean(abs(ts_test_data - arima_preds_ts))

# Displaying seasonal trend graph with forecasts
seasonal_trend_plot <- ggplot() +
  geom_line(data = time_series_ct, aes(x = as.Date(paste0(year, "-", month, "-01")), y = accident_count), color = "black") +
  geom_line(data = data.frame(date = as.Date(paste0(time_series_ct$year, "-", time_series_ct$month, "-01")), forecast = c(rep(NA, train_size), arima_preds_ts)),
            aes(x = date, y = forecast, linetype = "ARIMA"), color = "red", size = 1) +
  geom_line(data = data.frame(date = as.Date(paste0(time_series_ct$year, "-", time_series_ct$month, "-01")), forecast = c(rep(NA, train_size), linear_reg_preds_ts$mean)),
            aes(x = date, y = forecast, linetype = "Linear Regression"), color = "blue", size = 1) +
  labs(title = "Accident Counts Over Time (Connecticut)",
       x = "Date",
       y = "Accident Count") +
  theme_minimal() +
  theme(legend.position = "top", legend.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(name = "Forecast Type",
                        values = c("ARIMA" = "solid", "Linear Regression" = "dashed"),
                        labels = c("ARIMA Forecast", "Linear Regression Forecast")) +
  guides(linetype = guide_legend(override.aes = list(color = c("red", "blue"))))  # Set legend colors

# Printing the results
cat("Linear Regression Mean Absolute Error:", linear_reg_mae_ts, "\n")
cat("ARIMA Mean Absolute Error:", arima_mae_ts, "\n")

# Displaying the seasonal trend plot with forecasts
print(seasonal_trend_plot)
