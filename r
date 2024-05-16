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

# Selecting variables and converting date

king_time <- kingcountysales %>% 
                select(sale_date, sale_price, city) %>% 
                mutate(date = mdy(sale_date))

# Converting cities to factors

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

# Plotting monthly sales to find the best months to buy or sell homes in King County

plot_seasonal_diagnostics(king_monthly, .date_var = date, .value = median_sale)

# Best month to sell by median home price is June

# Best month to buy is January

# Removing data near housing crisis to see if it changes monthly medians

king_monthly_post2012 <- 
  king_monthly %>% 
  filter_by_time(.date_var = date, 
                 .start_date = "2013",
                 .end_date = "2024")

head(king_monthly_post2012, 12)

plot_seasonal_diagnostics(king_monthly_post2012, .date_var = date, .value = median_sale)

# This has similar results with June still being the best month to sell
# but December edging out January as the best month to buy.              
    
