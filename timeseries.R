# Time series analysis of home sales in King County Washington

# Install necessary package
install.packages("quantmod")
install.packages("timetk")
install.packages("fable")
install.packages("feasts")

# Upload data

library(readr)

kingcountysales <- read_csv("C:/Users/tjf4x/Desktop/R projects/King county home sales/kingcountysales_2000_2023.csv")

# Clean and transforming data

library(tidyverse)
library(lubridate)

glimpse(kingcountysales)

# Removing duplicates

duplicated(kingcountysales)

distinct(kingcountysales)

# selecting variables and converting date

king_time <- kingcountysales %>% 
  select(sale_date, sale_price, city) %>% 
  mutate(date = mdy(sale_date))

# converting cities to factors

unique(king_time$city)

king_time$city <- as.factor(king_time$city)

# Checking for missing values

sum(is.na(king_time))

# Calculating median by month

library(timetk)

king_monthly <- 
  king_time %>% 
  summarize_by_time(.date_var = date,
                    .by = "month",
                    median_sale = median(sale_price, na.rm = TRUE))

head(king_monthly, 12)

# Plotting monthly median sale price

plot_time_series(king_monthly, 
                 .date_var = date, 
                 .value = median_sale,
                 .interactive = TRUE,
                 .x_lab = "Monthy Data",
                 .y_lab = "Median Sale price")

# Plotting monthly sales to find best months to buy or sell homes in king county

plot_seasonal_diagnostics(king_monthly, .date_var = date, .value = median_sale)

# Best month to sell by median home price is June

# Best month to buy is January

# Removing data near housing crisis to see if it changes monthly medians

king_monthly_post2012 <- 
  king_monthly %>% 
  filter_by_time(.date_var = date, 
                 .start_date = "2013",
                 .end_date = "2024")

glimpse(king_monthly_post2012, 12)

plot_seasonal_diagnostics(king_monthly_post2012, .date_var = date, .value = median_sale)

# This has similar results with June still being the best month to sell
# but December edging out January as the best month to buy.


# Building a simple univariant forecast model
# -------------------------------------------

# Creating testing and training split

library(rsample)

king_monthly_split <- initial_time_split(king_monthly_post2012, prop = 130/136)

king_training <- training(king_monthly_split)
king_testing <- testing(king_monthly_split)

# converting data to a tibble and setting index as date

library(fable)
library(tsibble)

king_training <- 
  king_training %>% 
  mutate(date = yearmonth(date)) %>% 
  as_tsibble(index = date)

king_testing <- 
  king_testing %>% 
  mutate(date = yearmonth(date)) %>% 
  as_tsibble(index = date)

king_monthly_post2012 <- 
  king_monthly_post2012 %>% 
  mutate(date = yearmonth(date)) %>% 
  as_tsibble(index = date)

# Train the models

library(feasts)

king_fit <- 
  king_training %>% 
  model(stepwise = ARIMA(median_sale),
        search = ARIMA(median_sale, stepwise=FALSE))

# Viewing model
tidy(king_fit)

king_fit %>% 
  accuracy() %>% 
  arrange(MAPE)

# Checking forecast of training data vs full data set

king_fit %>% 
  forecast(h = "6 months") %>% 
  autoplot(king_monthly_post2012)

# final fit on full data set

king_final_fit <- 
  king_monthly_post2012 %>% 
  model(stepwise = ARIMA(median_sale),
        search = ARIMA(median_sale, stepwise=FALSE))

# Forcasting final model on full data set

king_final_fit %>% 
  forecast(h = "12 months") %>% 
  autoplot(king_monthly_post2012) +
  labs(x = "Month", y = "median home price")

king_final_fit %>% 
  forecast(h = "12 months") %>% 
  autoplot() +
  labs(x = "Month", y = "median home price")

# The data ends in December of 2023, but referencing median sales in recent
# we can see the forecast starts to get too optimistic. I added back in the housing 
# crisis data and retrained.

king_final_fit_full <- 
  king_monthly_t %>% 
  model(stepwise = ARIMA(median_sale),
        search = ARIMA(median_sale, stepwise=FALSE))

king_final_fit_full %>% 
  forecast(h = "12 months") %>% 
  autoplot(king_monthly_t) +
  labs(x = "Month", y = "median home price")

king_final_fit_full %>% 
  forecast(h = "12 months") %>% 
  autoplot() +
  labs(x = "Month", y = "median home price")

# Adding the full time series makes the model more conservative and makes the 
# forecast more accurate.

library(quantmod)

# Get mortgage rate data from FRED (MORTGAGE30US)
getSymbols("MORTGAGE30US", src = "FRED")

# Check the data
head(MORTGAGE30US)

# Plotting the data
chartSeries(MORTGAGE30US)

# Convert MORTGAGE30US to data frame
mortgage_data <- data.frame(
  date = index(MORTGAGE30US),          # Extract the date
  mortgage_rate = coredata(MORTGAGE30US) # Extract the values
)

# Convert to tibble for better compatibility with tidyverse functions
mortgage_data <- as_tibble(mortgage_data)

# Inspect the tibble
glimpse(mortgage_data)

# Get the start and end date of the home sales data
start_date <- min(king_monthly$date)
end_date <- max("2023-12-31")

# Filter mortgage data to match the home sales date range
mortgage_data_filtered <- mortgage_data %>%
  filter(date >= start_date & date <= end_date)

# Summarizing rate by month
mortgage_monthly <- 
  mortgage_data_filtered %>% 
  summarize_by_time(.date_var = date,
                    .by = "month",
                    mean_rate = mean(MORTGAGE30US, na.rm = TRUE))

head(mortgage_monthly, 12)

library(dplyr)

# Merge the filtered mortgage data with the home sales data
king_combined <- king_monthly %>%
  left_join(mortgage_monthly, by = "date")

# Inspect the combined data
glimpse(king_combined)

library(tsibble)

library(fable)

# Convert to tsibble
king_combined_ts <- king_combined %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Verify the tsibble
glimpse(king_combined_ts)

# Train the ARIMA model with mortgage rates as a regressor
king_fit_with_mortgage <- king_combined_ts %>%
  model(
    arima_with_mortgage = ARIMA(median_sale ~ xreg(mean_rate))
  )

# Check the model summary
king_fit_with_mortgage %>% tidy()

# Create rates for forecast
start_date2 <- min("2024-1-1")
end_date2 <- max("2024-9-30")

future_mean_rate <- mortgage_data %>%
  filter(date >= start_date2 & date <= end_date2)

# Summarizing rate by month
future_monthly <- 
  future_mean_rate %>% 
  summarize_by_time(.date_var = date,
                    .by = "month",
                    mean_rate = mean(MORTGAGE30US, na.rm = TRUE))

head(future_monthly, 12)

# Convert to tsibble
future_monthly_ts <- future_monthly %>%
  mutate(date = yearmonth(date)) %>%
  as_tsibble(index = date)

# Forecast the next 12 months using the external regressor
forecast_result <- king_fit_with_mortgage %>%
  forecast(new_data = future_monthly_ts)

# View the forecast
forecast_result %>% autoplot(king_combined_ts)

# View the forecast
forecast_result %>% autoplot()

# 
