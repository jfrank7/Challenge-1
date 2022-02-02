# Clear workspace
rm(list = ls())

library(tidyverse)

# Read CSV files (transactional is zipped so we use unz())
df_machine <- read.csv('./Data/machine_data.csv')
df_product <- read.csv('./Data/product_data.csv')
df_transactional <- read.csv(unz("./Data/transactional_data.csv.zip", 
                                 'transactional_data.csv'))

### 1.
# a. How many machines are there?
# There are 2495 unique machines.
cat('There are', length(df_machine$machine), 'unique machines.')

# b. What percentage of them are small?
# 38% of the machines are small.
cat(scales::percent(mean(df_machine$small_machine)), 'of the machines are small.')

# c. How do they distribute in terms of location type i.e. transport, petrol station? Products 
plyr::count(df_machine, 'location_type')
  
# d. How many products are there?
# There are 63 unique products.
cat('There are', length(df_product$product_name), 'unique products.')

# Which category has the highest number of products?
# Carbonates and energy drinks 13
head(plyr::count(df_product, 'category'), 3)

# e. Which category has the highest and lowest average price?
# Highest: Milk based 3.42
# Lowest: Sugar candy 2.3
df_product %>% 
  group_by(category) %>% 
  summarise_at(c('price'), mean) %>% 
  arrange(desc(price))

# And within snacks or drinks?
# Highest: drink 3.18
# Lowest: snack 2.42
df_product %>% 
  group_by(type_drink_snack) %>% 
  summarise_at(c('price'), mean) %>% 
  arrange(desc(price))

# f. Restricting the transactional data to March 2017
df <- df_transactional %>% 
  filter(lubridate::month(date) == 3)

# what’s the average daily items among small and big machines?
active_days<-df_transactional %>% 
  group_by(machine,day=lubridate::day(date)) %>%
  summarise(n = n()) %>%
  group_by(machine) %>%
  summarise(daysactive=n())

sold_machine<-df_transactional %>% 
  group_by(machine) %>%
  summarise(n = n())

df_machine$meansold<-sold_machine$n/active_days$daysactive

df_machine%>%
  group_by(small_machine) %>%
  summarise(mean(meansold))


# Why do you think there is such a difference? Give at least 2 possible reasons.
# Reason 1:
# Reason 2:

# Note: To calculate daily sales consider only the “active days” of a machine to exclude machine failures.
# For that, divide the number of items sold by a machine by the total number of “distinct” days. 

### 2
### 3
### 4
### 5
