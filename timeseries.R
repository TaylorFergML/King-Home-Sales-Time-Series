# Time series analysis of home sales in King County Washington

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

install.packages("timetk")

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

install.packages("fable")
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

install.packages("feasts")

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

# Fitting final model on full data set

king_final_fit %>% 
  forecast(h = "12 months") %>% 
  autoplot(king_monthly_post2012) +
  labs(x = "Month", y = "median home price")
