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

# splitting date into month and year, filtering variables

king_time <- king_time %>% 
  mutate(sale_month = month(date)) %>% 
  mutate(sale_year = year(date)) %>% 
  select(sale_month, sale_year, sale_price, city)

# converting cities to factors

unique(king_time$city)

king_time$city <- as.factor(king_time$city)

# Checking for missing values

sum(is.na(king_time))

# calculating median home sale by month and year

monthly_median <- king_time %>%
  group_by(sale_year, sale_month) %>%
  summarise(median_sale = median(sale_price))

# Removing data near the great recession
monthly_median <- monthly_median %>% filter(sale_year > 2013)

# reshaping dataframe

wide_monthly_median <- monthly_median %>% 
  pivot_wider(names_from = sale_month, values_from = median_sale)

library(zoo)
library(ggplot2)
autoplot(zoo(t(monthly_median$median_sale)), facets = NULL)



glimpse(king_time)                
    
