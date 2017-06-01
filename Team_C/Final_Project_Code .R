#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Tina Chen and Xiaoli Jin
# Kaggle Competition Name: Sberbank Russian Housing Market
# Kaggle Competition URL: https://www.kaggle.com/c/sberbank-russian-housing-market
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------

library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
library(ggplot2)
library(DAAG)
library(Metrics)

# 2. Load Data Files ------------------------------------------

train <- read.csv("Files/train.csv")
test <- read.csv("Files/test.csv")
macro <- read.csv("Files/macro.csv")
sample_submission <- read.csv("Files/sample_submission.csv")


# 3. Data Cleaning --------------------------------------

# To join 'macro' with 'test' and 'train' by "timestamp"
train$timestamp <- as.Date(train$timestamp)
macro$timestamp <- as.Date(macro$timestamp)
test$timestamp <- as.Date(test$timestamp)

# Create 'full_test' and 'full_train'
full_test <- left_join(test,macro, by = "timestamp")
full_train <- left_join(train,macro, by = "timestamp")


# We select the final data variables for both 'full_train' and 'full_test' that we would like to use in our model.
# We based it largely on the relevance of the data, the amount of NA's, and ones that showed a correlation in our EDA.
# Please refer to  report and EDA section for additional analyses of why we chose certain variables and ones we mutated.

full_train <- full_train %>%
  select(id, price_doc, full_sq, life_sq, floor, sub_area, sport_objects_raion, kremlin_km,
         school_education_centers_raion, area_m, healthcare_centers_raion, university_top_20_raion,
         park_km, metro_min_avto, industrial_km, incineration_km, kindergarten_km, green_zone_km,
         school_km, water_treatment_km, gdp_annual_growth, fixed_basket, gdp_annual, micex_rgbi_tr,
         balance_trade_growth, deposits_value) %>%
  filter(full_sq >= life_sq & full_sq >= 5 & full_sq < 1000) %>%
  subset(select = -life_sq)


full_test <- full_test %>%
  select(id, full_sq, floor, sub_area, sport_objects_raion, kremlin_km,
         school_education_centers_raion, area_m, healthcare_centers_raion, university_top_20_raion,
         park_km, metro_min_avto, industrial_km, incineration_km, kindergarten_km, green_zone_km,
         school_km, water_treatment_km, gdp_annual_growth, fixed_basket, gdp_annual, micex_rgbi_tr,
         balance_trade_growth, deposits_value) %>%
  mutate(price_doc = 0)


# Check the percentage of NA in the current data and omit the NA.
# For e.g., floor has less than 1% missing,so we are going to omit it anyway
# It's no coincidence that the variables have close to 0% of missing data.
# We did not use 'build_year' because >44% of the data was missing.
apply(full_train, 2, function(x){mean(is.na(x))})
full_train <- na.omit(full_train)

# Important note: In train data, there are 146 sub_area(146 factor).
# However, test data only has 145 sub_area(145 factor).
# We wrote a for-loop that pointed us to the problem and and got rid of extra sub_area in train data.
train_area <- unique(full_train$sub_area)
test_area <- unique(full_test$sub_area)
for(i in train_area) {
  if(i %in% test_area == FALSE){
    print(i)
  }
}

# The extra sub_area is Poselenie Klenovskoe
# To make glmnet work, we removed it.
full_train <- full_train %>%
  #filter(full_sq >= life_sq & full_sq >= 5 & full_sq < 1000) %>%
  filter(sub_area != 'Poselenie Klenovskoe') %>%
  mutate(sub_area = as.character(sub_area)) %>%
  mutate(sub_area = as.factor(sub_area))

# 4. Top 4-5 Visualizations/Tables of EDA ---------------------------------

# 1. Log price_doc vs. density
# Creating a density plot will help check the spread of the price (log transformation).
ggplot(aes(x = log(price_doc)), data = train) +
  geom_density()

# 2. First manipulated the price per sq m, instead of entire house. Then plotted it from most expensive to
# cheapest by district
train %>%
  filter(build_year>1690)%>%
  mutate(price_per_sq_m = price_doc/full_sq,
         psqm_life = price_doc/life_sq) %>%
  group_by(sub_area) %>%
  summarize(n=n(),
            price_sqm = mean(price_per_sq_m)) %>%
  ggplot(aes(x=reorder(sub_area, price_sqm), y=price_sqm, fill = n)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(y='mean price per district', x='sub') +
  ggtitle('Mean Price per District')

# 3. Amenities
# There are a lot of predictors regarding nearby amenities, including hospital, schools, shopping center, etc
# We placed 14 of the variables into a LASSO regression, trying to identify which are the most important predictors
model_formula_nearby_amenity <- as.formula(price_doc ~ area_m + raion_popul + green_zone_part + indust_part +
                                             children_preschool + children_school + school_education_centers_raion +
                                             healthcare_centers_raion + university_top_20_raion +
                                             sport_objects_raion + additional_education_raion +
                                             culture_objects_top_25_raion + shopping_centers_raion + office_raion)

X <- model.matrix(model_formula_nearby_amenity, data = train)[, -1]
y <- train$price_doc

lambda_values <- 10^seq(0,20, length = 200)

model_LASSO_nearby_amenities <- glmnet(X, y, alpha = 1, lambda = lambda_values)

model_LASSO_nearby_amenities %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio)) %>%
  View()

# As shown in ggplot, the important predictors are: sports_objects_raion,
# school_education_centers_raion, area_m, healthcare_center_raion and university_top_20_raion

model_LASSO_nearby_amenities %>%
  tidy() %>%
  tbl_df() %>%
  # Lambda values on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()

ggplotly()

# 4. Finding the best variables in macro
macro <- macro %>%
  dplyr::rename(
    rent_price_four_plus_room_bus = `rent_price_4.room_bus`,
    rent_price_three_room_bus = `rent_price_3room_bus`,
    rent_price_two_room_bus = `rent_price_2room_bus`,
    rent_price_one_room_bus = `rent_price_1room_bus`
  )

# Merge train and macro by timestamp
train$timestamp <- as.Date(train$timestamp)
total <- left_join(train,macro, by = "timestamp")

# Since there are so many NA's in macro data, the following for-loop selects all predictors with less than 1/10 NA's
variable_less_na <- vector()
for(i in 1:ncol(macro)) {
  x = macro[i]
  x = na.omit(x)
  if(nrow(x) >= 9/10 * nrow(macro)){
    variable_less_na <- c(variable_less_na,colnames(x[1]))
  }
}

# Variable_less_na <- as.factor(variable_less_na)
macro_formula <- paste("price_doc", '~', paste(variable_less_na, collapse=' + '))
macro_formula <- as.formula(macro_formula)
#model_macro <- lm(macro_formula, data = total)

# Use LASSO to select important variables among the 23 variables with less than 1/10 na
X <- model.matrix(macro_formula, data = total)[, -1]
y <- total$price_doc

lambda_values <- 10^seq(0,20, length = 1000)

model_LASSO_macro <- glmnet(X, y, alpha = 1, lambda = lambda_values)

model_LASSO_macro %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()

ggplotly()

#As shown in the graph, the macro variables that are both statistically important and have less than 1/10 na value are:
#gdp_annual_growth, fixed_basket, gdp_annual, micex_rgbi_tr, balance_trade_growth, deposits_growth, mortgage_rate,average_provision_of_build_contract
#We plan to choose the top five!

# Manipulating Data
# Here, we are manipulating the data to see what is the density for population in terms of raion, the density of working,
EDA_train <- train %>%
  mutate(
    year = lubridate::year(timestamp),
    month = lubridate::month(timestamp),
    non_life_sq = full_sq - life_sq,
    popul_dens_raion = raion_popul / area_m, # Population density
    full_dens = full_all / area_m,
    young_dens = young_all / area_m,
    work_dens = work_all / area_m
  ) %>%
  select(price_doc, year, month, non_life_sq, popul_dens_raion, full_dens, young_dens, work_dens)

# 5. #Xiaoli's Russian professor told her that people in Moscow don't want to live on the ground floor or the top floor
#because Moscow is very humid so we thought it'd be interesting
#We also need to get rid of wrong values, e.g. when floor > max floor
train_floor <- train %>%
  select(price_doc, floor,max_floor) %>%
  na.omit() %>%
  filter(floor <= max_floor) %>%
  #for the purpose of ggplot
  mutate(floor_fac = as.factor(floor))

# We use boxplot for a better visulization
ggplot(train_floor,aes(y = log(price_doc), x = floor_fac)) + geom_boxplot()

# We want to know if it is worth it to add max_floor as another predictor,
#so we ran two cross_validation models, one with max_floor and one without
#It turned out that including max floor improve overall ms 0.01e + 13
#Howver, We dicided not to include it, bacause there are too many na in max floor
cv.lm(train_floor,form.lm = formula(price_doc ~ floor), m = 10)
cv.lm(train_floor,form.lm = formula(price_doc ~ floor + max_floor), m = 10)

# 5. Cross-Validation of Final Model --------------------------------------

# Create the model. We set up (y, X) here based on glmnet's format.
model_formula <- as.formula(price_doc ~ full_sq + floor + sub_area + sport_objects_raion + kremlin_km+
                              school_education_centers_raion + area_m + healthcare_centers_raion + university_top_20_raion+
                              park_km + metro_min_avto + industrial_km + incineration_km + kindergarten_km + green_zone_km+
                              school_km + water_treatment_km +  gdp_annual_growth + fixed_basket + gdp_annual + micex_rgbi_tr+
                              balance_trade_growth + deposits_value)

X <- model.matrix(model_formula, data = full_train)[, -1]
y <- full_train$price_doc


length_l = 100
lambda_values <- 10^seq(-5,10, length = length_l)

n_folds <- 10
full_train <- full_train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)


lambda_tracker <- NULL
score_all_lambda <- NULL

count = 1

for(l in lambda_values) {
  lambda_tracker[count] <- l
  results_all_folds <- NULL

  for(i in 1:n_folds){
    # 1. Create pseudo train/test based on folding scheme
    pseudo_train <- full_train %>%
      filter(fold != i)
    pseudo_test <- full_train %>%
      filter(fold == i)
    # Needed for glmnet() methods:
    pseudo_train_X <- model.matrix(model_formula, data = pseudo_train)[, -1]
    pseudo_train_y <- pseudo_train$price_doc
    pseudo_test_X <- model.matrix(model_formula, data = pseudo_test)[, -1]

    # 2. Fit model to pseudo-train
    model_LASSO <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 1, lambda = l)

    # 3. Get predictions
    LASSO_predictions <- model_LASSO %>%
      predict(newx=pseudo_test_X, s=l)

    # 4. Compute Score according to RMSLE
    pseudo_test <- pseudo_test %>%
      mutate(predictions = as.vector(LASSO_predictions))

    # 5.get rid of nagative values(since our scoring mechnism involves log, no nagetive value is allowed)
    for(m in pseudo_test$predictions) {
      if(m < 0) {
        pseudo_test$predictions[m] = mean(pseudo_test$predictions) * 1/2
      }
    }

    RMLSE_one_fold <- mean((log(pseudo_test$predictions + 1) - log(pseudo_test$price_doc + 1))^2)

    #this is the built-in function that makes our scores funky
    #RMLSE_one_fold <- rmsle(pseudo_test$price_doc,pseudo_test$predictions)

    results_all_folds[i] <- RMLSE_one_fold
  }
  print(count)
  score_all_lambda[count] <-mean(results_all_folds)
  count <- count + 1
}

cv_result <- data_frame(
  lambda = lambda_tracker,
  score = score_all_lambda
)

# Extract the best lambda value, i.e. lambda with the lowest score
least_score <- cv_result$score[1]
lambda_star <- cv_result$lambda[1]
for(k in 1: nrow(cv_result)){
  if(cv_result$score[k] < least_score){
    least_score = cv_result$score[k]
    lambda_star = cv_result$lambda[k]
  }
}

lambda_star


# 6. Create Submission ----------------------------------------------------

# Build the model using the optimal lambda from CV
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_star)

# Now, we will apply the model to 'test' data
X_new <- model.matrix(model_formula, data = full_test)[, -1]

# LASSO Predictions
LASSO_predictions <- model_LASSO %>%
  predict(newx=X_new, s=lambda_star)


# Create submission file
submission_old <- full_test %>%
  mutate(price_doc = as.vector(LASSO_predictions)) %>%
  select(id, price_doc) %>%
  mutate(price_doc = replace(price_doc, price_doc < 0, NA))

# Note: There are 3 negative values. We originally changed it to 0's and decided maybe just finding the mean
# or finding the absolute would be better.But we decided to do more investigation to figure it out...
#submission$price_doc [submission$price_doc < 0] <- 0
test_negatives <- full_test %>%
  mutate(price_doc = as.vector(LASSO_predictions)) %>%
  select(id, price_doc, sub_area) %>%
  filter(price_doc <= '0')

test_negatives_by_sub_area <- full_train %>%
  select(id, price_doc, sub_area) %>%
  group_by(sub_area) %>%
  summarize(mean = mean(price_doc, na.rm = TRUE))

test_negative_join <- left_join(test_negatives_by_sub_area, test_negatives, by = "sub_area")
test_negative_join <- na.omit(test_negative_join)

# Here, you can see the mean values by sub_area and we will replace the negative values with the mean instead.
submission_new <- left_join(submission_old, test_negative_join, by = "id") %>%
  mutate(price_doc = mean) %>%
  select(id, price_doc)

# Keep only with values
submission_new <- na.omit(submission_new)
submission_old <- na.omit(submission_old)

#Merge submission and submission new together
submission <- rbind(submission_old,submission_new)
submission <- submission %>%
  arrange(desc(id))


# Write submission
write_csv(submission, "LASSO_submission.csv")

# Leaderboard: 1494/ 1857
# RMSLE Score: 0.37955


# 7. Extras ---------------------------------------------------------------

# ===========================================================================
# Section A: Ridge Model

# From Sections 1 - 4, run from LASSO Model.
# ------------------------------------------
# Section 5: Prediction starts here

#build the model using optimal lambda
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_star)

#apply the model to test data
X_new <- model.matrix(model_formula, data = full_test)[, -1]

ridge_predictions <- model_ridge %>%
  predict(newx=X_new, s=lambda_star)

#create submission file
#Note: there are three negative values in our prediction. I just randomly change them to 0 for now
# Create submission file
submission_old <- full_test %>%
  mutate(price_doc = as.vector(ridge_predictions)) %>%
  select(id, price_doc) %>%
  mutate(price_doc = replace(price_doc, price_doc < 0, NA))

# Note: Like before, we have 3 negative values. We find the mean of the price_doc by the sub_area
#submission$price_doc [submission$price_doc < 0] <- 0
test_negatives <- full_test %>%
  mutate(price_doc = as.vector(ridge_predictions)) %>%
  select(id, price_doc, sub_area) %>%
  filter(price_doc <= '0')

test_negatives_by_sub_area <- full_train %>%
  select(id, price_doc, sub_area) %>%
  group_by(sub_area) %>%
  summarize(mean = mean(price_doc, na.rm = TRUE))

test_negative_join <- left_join(test_negatives_by_sub_area, test_negatives, by = "sub_area")
test_negative_join <- na.omit(test_negative_join)

# Here, you can see the mean values by sub_area and we will replace the negative values with the mean instead.
submission_new <- left_join(submission_old, test_negative_join, by = "id") %>%
  mutate(price_doc = mean) %>%
  select(id, price_doc)

# Keep only with values
submission_new <- na.omit(submission_new)
submission_old <- na.omit(submission_old)

#Merge submission and submission new together
submission <- rbind(submission_old,submission_new)
submission <- submission %>%
  arrange(desc(id))

write_csv(submission, "Ridge_submission.csv")

# RMSLE Score: 0.39919 (Not an improvement from the best score)

# ===========================================================================

# Section B: CART
# We have included the entire code.

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
library(ggplot2)
library(DAAG)
library(Metrics)

# 2. Load Data Files
train <- read.csv("~/Desktop/train.csv")
test <- read.csv("~/Desktop/test.csv")
macro <- read.csv("~/Desktop/macro.csv")
sample_submission <- read.csv("~/Desktop/sample_submission.csv")


# 3. Data Cleaning --------------------------------------

# To join 'macro' with 'test' and 'train' by "timestamp"
train$timestamp <- as.Date(train$timestamp)
macro$timestamp <- as.Date(macro$timestamp)
test$timestamp <- as.Date(test$timestamp)

# Create 'full_test' and 'full_train'
full_test <- left_join(test,macro, by = "timestamp")
full_train <- left_join(train,macro, by = "timestamp")


# We select the final data variables for both 'full_train' and 'full_test' that we would like to use in our model.
# We based it largely on the relevance of the data, the amount of NA's, and ones that showed a correlation in our EDA.
# Please refer to  report and EDA section for additional analyses of why we chose certain variables and ones we mutated.
full_train <- full_train %>%
  select(id, price_doc, full_sq, life_sq, floor, sub_area, sport_objects_raion, kremlin_km,
         school_education_centers_raion, area_m, healthcare_centers_raion, university_top_20_raion,
         park_km, metro_min_avto, industrial_km, incineration_km, kindergarten_km, green_zone_km,
         school_km, water_treatment_km, gdp_annual_growth, fixed_basket, gdp_annual, micex_rgbi_tr,
         balance_trade_growth, deposits_value) %>%
  filter(full_sq >= life_sq & full_sq >= 5 & full_sq < 1000) %>%
  subset(select = -life_sq)


full_test <- full_test %>%
  select(id, full_sq, floor, sub_area, sport_objects_raion, kremlin_km,
         school_education_centers_raion, area_m, healthcare_centers_raion, university_top_20_raion,
         park_km, metro_min_avto, industrial_km, incineration_km, kindergarten_km, green_zone_km,
         school_km, water_treatment_km, gdp_annual_growth, fixed_basket, gdp_annual, micex_rgbi_tr,
         balance_trade_growth, deposits_value) %>%
  mutate(price_doc = 0)


# Check the percentage of NA in the current data and omit the NA.
apply(full_train, 2, function(x){mean(is.na(x))})
full_train <- na.omit(full_train)

# Important note: In train data, there are 146 sub_area(146 factor).
# However, test data only has 145 sub_area(145 factor).
# We wrote a for-loop that pointed us to the problem and and got rid of extra sub_area in train data.
train_area <- unique(full_train$sub_area)
test_area <- unique(full_test$sub_area)
for(i in train_area) {
  if(i %in% test_area == FALSE){
    print(i)
  }
}

# The extra sub_area is Poselenie Klenovskoe
# To make glmnet work, we removed it.
full_train <- full_train %>%
  #filter(full_sq >= life_sq & full_sq >= 5 & full_sq < 1000) %>%
  filter(sub_area != 'Poselenie Klenovskoe') %>%
  mutate(sub_area = as.character(sub_area)) %>%
  mutate(sub_area = as.factor(sub_area))

# Create the model.
model_formula <- as.formula(price_doc ~ full_sq + floor + sub_area + sport_objects_raion + kremlin_km+
                              school_education_centers_raion + area_m + healthcare_centers_raion + university_top_20_raion+
                              park_km + metro_min_avto + industrial_km + incineration_km + kindergarten_km + green_zone_km+
                              school_km + water_treatment_km +  gdp_annual_growth + fixed_basket + gdp_annual + micex_rgbi_tr+
                              balance_trade_growth + deposits_value)

# 4. Cross-Validation of Final Model --------------------------------------

length_l = 10
maxdepth <- seq(1,10, length = length_l)

n_folds <- 10
full_train <- full_train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

depth_tracker <- NULL
score_all_depth <- NULL

count = 1

for(l in maxdepth) {
  depth_tracker[count] <- l
  results_all_folds <- NULL

  for(i in 1:n_folds){
    # 1. Create pseudo train/test based on folding scheme
    pseudo_train <- full_train %>%
      filter(fold != i)
    pseudo_test <- full_train %>%
      filter(fold == i)

    # 2. Fit model to pseudo-train
    knobs <- rpart.control(maxdepth = l)
    model_CART <- rpart(model_formula, data = pseudo_train, control=knobs)


    # 3. Get predictions
    cart_predictions <- predict(model_CART,newdata=pseudo_test)

    # 4. Compute Score according to RMSLE
    pseudo_test <- pseudo_test %>%
      mutate(predictions = as.vector(cart_predictions))

    # 5. Get rid of negative values(since our scoring mechnism involves log, no nagetive value is allowed)
    for(m in pseudo_test$predictions) {
      if(m < 0) {
        pseudo_test$predictions[m] = mean(pseudo_test$predictions) * 1/2
      }
    }

    RMLSE_one_fold <- mean((log(pseudo_test$predictions + 1) - log(pseudo_test$price_doc + 1))^2)
    #RMLSE_one_fold <- rmsle(pseudo_test$price_doc,pseudo_test$predictions)

    results_all_folds[i] <- RMLSE_one_fold
  }
  print(count)
  score_all_depth[count] <-mean(results_all_folds)
  count <- count + 1
}

cv_result <- data_frame(
  depth = depth_tracker,
  score = score_all_depth
)

# Extract the best lambda value, i.e. lambda with the lowest score
least_score <- cv_result$score[1]
depth_star <- cv_result$depth[1]
for(k in 1:nrow(cv_result)){
  if(cv_result$score[k] < least_score){
    least_score = cv_result$score[k]
    depth_star = cv_result$depth[k]
  }
}

depth_star

# 5. Create Submission ----------------------------------------------------

# Build the model
knobs <- rpart.control(maxdepth = depth_star)
model_CART <- rpart(model_formula, data = full_train, control=knobs)

# Cart Predictions
CART_final_predictions <- predict(model_CART, newdata = full_test)

#Submission
submission <- full_test %>%
  mutate(price_doc = CART_final_predictions) %>%
  select(id, price_doc)

write_csv(submission, "CART_submission.csv")

#Score: 0.37827

# ===========================================================================

# Section C

#Additional EDA

# 1. New variables
EDA_train <- train %>%
  mutate(
    year = lubridate::year(timestamp),
    month = lubridate::month(timestamp),
    non_life_sq = full_sq - life_sq,
    popul_dens_raion = raion_popul / area_m, # Population density
    full_dens = full_all / area_m,
    young_dens = young_all / area_m,
    work_dens = work_all / area_m
  )

# 2. Groups it by sub_area and separates by year: both mean and median
prices_by_sub_area_year <- train %>%
  group_by(sub_area, build_year) %>%
  summarise(
    mean_price_by_area = mean(price_doc, na.rm = TRUE),
    median_price_by_area = median(price_doc, na.rm = TRUE)
  )

# 3. This is the mean house price by the number of sports objects in Raion. There is a positive correlation
train %>%
  group_by(sub_area) %>%
  summarize(sport_objects=mean(sport_objects_raion), mean_price=mean(price_doc)) %>%
  ggplot(aes(x=sport_objects, y=mean_price)) +
  geom_point() +
  geom_smooth(method='lm')

# 4. Distance to kremlin_km and price doc
# This is saying the the farther the house from kremlin, the cheaper it is!
ggplot(aes(x=kremlin_km, y=log(price_doc)), data=train) +
  geom_point(alpha = 0.4) +
  geom_smooth(method='lm', color='red')
# 5. Full_sq and life_sq
# Clearly, the size of the property is related to price. The lm model (price~full_sq) gives a R-squared value of 0.341.
# To explore a better model, we calculate non_life_sq by substracting life_sq from full_sq, and we use non_life_sq
# and life_sq as our predictive variables. Interestingly, the new model returned exactly the same R-squared value and MSE
train_size <- train %>%
  select(price_doc,full_sq,life_sq) %>%
  mutate(non_life_sq = full_sq - life_sq) %>%
  na.omit() %>%
  # Omit wrong values
  filter(full_sq >= life_sq & full_sq >= 5 & full_sq < 1000)

model_full <- lm(train_size$price_doc ~ train_size$full_sq)
model_life_nonlife <- lm(train_size$price_doc ~ train_size$life_sq + train_size$non_life_sq)

# The R squared of model full_sq is 0.341
summary(model_life_nonlife)
# MSE_full_sq:1.63e+13
cv.lm(train_size,form.lm = formula(price_doc ~ life_sq+non_life_sq), m = 10)
# The R squared of model life/non_life is 0.341
summary(model_full)
# MSE_life_non_life:1.63e+13
cv.lm(train_size,form.lm = formula(price_doc ~ full_sq), m = 10)

# 6. EDA on year_build
# The given data is not ready for use: first, the years are shown in factor, not in numeric value.
# Second, there are a lot of wrong values in the column, eg. 0, 1 ,3, 4965

# Convert build_year from factor to numeric value
train$build_year <- as.numeric(train$build_year)

# Omit na and filter wrong value
train_year <- train %>%
  select(price_doc, build_year) %>%
  na.omit() %>%
  filter(build_year > 1691) %>%
  filter(build_year < 2017)

# Oops, this graph is not so indicative since price is too large... log to the rescue!
ggplot(train_year,aes(x=build_year, y=price_doc)) + geom_point()

# The graph is still not very indicative, because too many dots cluter around 1950 - 2010
ggplot(train_year,aes(x = build_year, y= log(price_doc))) + geom_point()
# We reset the range of x axis to 1950 - 2017. From the graph, we are not sure if there is a
# strong positive relationship between price and year build
ggplot(train_year,aes(x = build_year, y= log(price_doc))) + geom_point() + coord_flip(xlim = c(1950, 2017))

# The p value is under 0.05, but also notice that R squared is small(0.001164).
# Guess that's reasonable, since build_year along cannot predict house price
# We decided not to include year_build, given that almost half of the data is missing
model_year <- lm(train_year$price_doc ~ train_year$build_year)
summary(model_year)


# 7. More EDA on sub area, which is the district the property belongs in
train_district <- train %>%
  select(price_doc, sub_area) %>%
  na.omit()

# From the boxplot, we saw that sub_area does make a differnece in price!
# It is important to include sub_area in our model
ggplot(train_district,aes(y = log(price_doc), x = sub_area)) + geom_boxplot()


# 8. State (condition) of the house. 1, 2, 3, 4
# State is important, as shown in the boxplot, but the problem is, almost half
# of the state information is missing
train_state <- train %>%
  select(price_doc, state) %>%
  filter(state <= 4) %>%
  na.omit() %>%
  mutate(state = as.factor(state))
ggplot(train_state,aes(y = log(price_doc), x = state)) + geom_boxplot()

# 9. Material of the house
# From the graph, difference of material doesn't lead to much differnce of price
# We are not going to use this predictor
train_material <- train %>%
  select(price_doc, material) %>%
  na.omit() %>%
  mutate(material = as.factor(material))
ggplot(train_material,aes(y = log(price_doc), x = material)) + geom_boxplot()

# 10. Product type
# From the graph we can see that the price patterns differ a lot based on product type
train_product_type <- train %>%
  select(price_doc, product_type) %>%
  na.omit()
ggplot(train_product_type,aes(y = log(price_doc), x = product_type)) + geom_boxplot()

# 11. Number of rooms in the house
#Note: The catagorical variable "number of room" has a lot of wrong values that are hard to get rid of
#e.g. a 40 full_sq house with 19 number of room (as can be seen in the graph, there exist many outliers)
#Therefore, we are not going to include this variable in our model
ggplot(train,aes(y = full_sq, x = num_room)) + geom_point(na.rm = TRUE) + coord_flip(ylim = c(0, 300))

# 12. This is the mean and median house prices by sub_area (similar to #2)
prices_by_sub_area <- train %>%
  group_by(sub_area) %>%
  summarise(
    mean_price_by_area = mean(price_doc, na.rm = TRUE),
    median_price_by_area = median(price_doc, na.rm = TRUE)
  )

# 13. Neighborhood features

model_formula_neighborhood_feature <- as.formula(price_doc~full_all+ young_all + work_all + ekder_all + build_count_1921.1945 + build_count_1946.1970 + build_count_1971.1995 + build_count_after_1995 + kindergarten_km + metro_min_walk + school_km + park_km + green_zone_km + industrial_km + water_treatment_km + incineration_km + railroad_station_walk_min)

train_nf <- train %>%
  select(price_doc, full_all, young_all, work_all, ekder_all, build_count_1921.1945, build_count_1946.1970, build_count_1971.1995, build_count_after_1995, kindergarten_km, metro_min_walk, school_km, park_km, green_zone_km, industrial_km, water_treatment_km, incineration_km, railroad_station_walk_min) %>%
  na.omit()

X <- model.matrix(model_formula_neighborhood_feature, data = train)[, -1]
y <- train_nf$price_doc

lambda_values <- 10^seq(0,25, length = 3000)

model_LASSO_neighborhood_feature <- glmnet(X, y, alpha = 1, lambda = lambda_values)

model_LASSO_neighborhood_feature %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_point()
ggplotly()

#As shown in the plot, the important predictors are: park_km, metro_min_walk, industrial_km
#incineration_km, kindergarden_km, green_zone_km, school_km, and water_treatment_km.

# C1. Built-in CV
#cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

#cvfit %>%
#  glance()
#lambda_star <- cvfit %>%
#  glance() %>%
#  .[["lambda.min"]]
#lambda_star
