# Clear workspace
rm(list = ls())
library(dplyr)
library(tidyverse)
# install.packages("zoo")   Install & load zoo package
library("zoo")

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
active_days <- df %>% 
  group_by(machine, date) %>%
  summarise() %>%
  group_by(machine) %>%
  summarise(daysactive = n())

sold_machine <- df %>% 
  group_by(machine) %>%
  summarise(transactions = n())

unique_df <- df %>% 
  group_by(machine) %>% 
  summarise()

unique_machine <- df_machine%>% 
  filter(machine %in% unique_df$machine)

unique_machine$meansold <- sold_machine$transactions / active_days$daysactive

unique_machine %>%
  group_by(small_machine) %>%
  summarise(mean(meansold))

# Why do you think there is such a difference? Give at least 2 possible reasons.
# Reason 1: Because bigger machines may have more and more varied product that results in more sales
# Reason 2: Because bigger machines are visually more attractive and thus generate more interest/sales

#5
df5<- df_machine %>%
        summarise(machine,small_machine,income_average,total_number_of_routes_600,num_hotels_45,
          daily_passengers_missing = ifelse(is.na(train_AvgDailyPassengers),1,0),num_vendex_nearby_300)
df5.1<- df_transactional %>% 
          group_by(machine) %>% 
          summarise(transactions=n())

df5.2 <- merge(x=df5,y=df5.1,by='machine',all.x=TRUE)

hist(df5.2$income_average) 
abline(v=mean(df5.2$income_average, na.rm = TRUE), col="blue") 
abline(v=median(df5.2$income_average, na.rm = TRUE), col="green")

mean(df5.2$income_average, na.rm = TRUE)
median(df5.2$income_average, na.rm = TRUE)

#checking for missing values
cbind(
  lapply(
    lapply(df5.2, is.na)
    , sum)
)
#filling missing values with the columns medians
df5.2 <- na.aggregate(df5.2,FUN = median)  

vendex_lm_model <- lm(formula = transactions ~ small_machine + income_average + total_number_of_routes_600 +
                        num_hotels_45 + daily_passengers_missing + num_vendex_nearby_300 , data = df5.2)
summary(vendex_lm_model)

set.seed(2323)
df5.2_split <- initial_split(df5.2, prop = .8)
df5.2_train <- training(df5.2_split)
df5.2_test <- testing(df5.2_split)

df5.2_lm_model <- linear_reg() %>% 
  set_engine('lm') %>%
  set_mode('regression')

### Pre-processing -------------------------------------------------------------
# A complete list of pre-processor steps can be found at:
# https://recipes.tidymodels.org/reference/index.html
df5.2_recipe <- recipe(transactions ~ small_machine + income_average + total_number_of_routes_600 +
                         num_hotels_45 + daily_passengers_missing + num_vendex_nearby_300 , data = df5.2) %>% 
  # step_normalize(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal())

# Inspect the pre-processed data
pre_processed <- df5.2_recipe %>% 
  prep() %>% 
  bake(new_data = df5.2_train) %>% 
  print()

### The workflow ---------------------------------------------------------------
df5.2_lm_workflow <- 
  workflow() %>% 
  add_model(df5.2_lm_model) %>% 
  add_recipe(df5.2_recipe)

### Fitting the model to train dataset -----------------------------------------
df5.2_lm_fit <- fit(df5.2_lm_workflow, df5.2_train)

# Summary of results
df5.2_lm_fit %>% extract_fit_engine() %>% summary()
df5.2_lm_fit %>% extract_fit_engine() %>% confint()

# Extraction of coefficients in a tidy way
tidy(df5.2_lm_fit)
glance(df5.2_lm_fit)

#a.TOAL_NUMBER_OF_ROUTES_60O and num_vendex_nearby_300 ARE NOT STATISTICALLY SIGNIFICANT
#b.
#c. -69.8
#d. -13.3

mean(df5.2$transactions)
median(df5.2$transactions)
min(df5.2$transactions)
quantile(df5.2$transactions, c(0.2,0.8))




### Predicting -----------------------------------------------------------------
train_lm_predictions <- df5.2_train %>% bind_cols(predict(object = df5.2_lm_fit, new_data = df5.2_train))
#predictions_vs_actual_plot(df = train_lm_predictions, x = transactions, y = .pred, title = 'Linear Regression: Train')

test_lm_predictions <- df5.2_test %>% bind_cols(predict(object = df5.2_lm_fit, new_data = df5.2_test)) 
#predictions_vs_actual_plot(df = test_lm_predictions, x = transactions, y = .pred, title = 'Linear Regression: Test')

### Assessing performance ------------------------------------------------------
rmse(train_lm_predictions, truth = transactions, estimate = .pred)
rmse(test_lm_predictions, truth = transactions, estimate = .pred)

# We can create a set of measures to estimate performance
lm_metrics <- metric_set(rmse, rsq, mae)
lm_metrics(train_lm_predictions, truth = transactions, estimate = .pred)
lm_metrics(test_lm_predictions, truth = transactions, estimate = .pred)

#e.
real_lm_predictions <- rbind(train_lm_predictions, test_lm_predictions)
q<- quantile(real_lm_predictions$.pred,c(0.2,0.8), na.rm=TRUE)
q
q[[2]]/q[[1]]

#f. I simulated the data given by the exercice: LOCATION 2 IS WAY BETTER.
loc <- data.frame (small_machine  = c(1, 1),
                        income_average = c(mean(df5.2$income_average), mean(df5.2$income_average)),
                        total_number_of_routes_600 = c(20, 10),
                        num_hotels_45 = c(2, 0),
                        daily_passengers_missing = c(1, 0),
                        num_vendex_nearby_300 = c(0, 3))

bind_cols(predict(object = df5.2_lm_fit, new_data = loc))

# f. Given the following 2 locations for a big machine:
#   i. Supermarket entrance, 2 nearby hotels of 4 stars, 20 transport routes, no
# nearby machines
# ii. Transport station, no nearby hotels of 4 or 5 stars, 10 transport routes
# nearby, 3 nearby Vendex machines
# Which location would you choose and why?