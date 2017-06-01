##########################################################################################
#                           STATISTICAL LEARNING FINAL PROJECT
#                           By: Alfred Hurley and Elias Van Sickle
#                           Specially made for: Professor Kim AKA Rudeboy
###########################################################################################

#------ LOAD PACKAGES ---------------------------------------------------------------------
library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
library(stringr)
library(Metrics)

#------ LOAD TRAIN / TEST DATA ------------------------------------------------------------
train <- readr::read_csv("Files/train.csv")
test <- readr::read_csv("Files/test.csv")

#------- DATA CLEANING --------------------------------------------------------------------
#Format train dates
dates <- rep(0, 10886)
for(i in 1:length(train$datetime)) {
  x <- as.character(train$datetime[i])
  if(nchar(x) < 11) {
    x <- paste(x, "00:00:00")
  }
  dates[i] <- x
}

# Add hours column to training data
t <- strftime(dates, format="%H") %>%
  as.numeric((t))
train <- train %>%
  mutate(t) %>%
  rename("hours" = t)


# Format test dates
dates <- rep(0, 6493)
for(i in 1:length(test$datetime)) {
  x <- as.character(test$datetime[i])
  if(nchar(x) < 11) {
    x <- paste(x, "00:00:00")
  }
  dates[i] <- x
}
# Add hours column to test data
t <- strftime(test$datetime, format="%H") %>%
  as.numeric((t))
test <- test %>%
  mutate(t) %>%
  rename("hours" = t)

test <- test %>%
  mutate(datetime = dates)


#------- EXPLORATORY DATA ANALYSIS ---------------------------------------------------------

#Season Analysis
ggplot(train, aes(x=season, y=count)) +
  geom_bar(stat = "identity")

#Weather Analysis
#1: Clear, Few clouds, Partly cloudy, Partly cloudy
#2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
ggplot(train, aes(x=weather, y=count)) +
  geom_bar(stat = "identity")

#Datetime Analysis
date <- train$datetime
count <- train$count
deg_free <- 8

model_spline <- smooth.spline(x=date, y=count, df=deg_free)

model_spline %>%
  augment() %>%
  ggplot(aes(x=date)) +
  geom_point(aes(y=count)) +
  geom_line(aes(y=.fitted), col="blue", size=2)

#Hour Analysis
ggplot(train, aes(x=hours, y=count)) +
  geom_point()

#Work Day Analysis
#1 = Workday
#0 = Other
ggplot(train, aes(x=workingday, y=count)) +
  geom_bar(stat = "identity")


#------- Principle Component Analysis ---------------------------------------------------------

# Combine temperature and "feels like temperature" into one variable to achieve a slightly simpler model
temps <- test %>%
  select(temp, atemp)

temps_recenter <- temps %>%
  mutate(
    temp = temp - mean(temp),
    atemp = atemp - mean(atemp)
  )

ggplot(temps_recenter, aes(x=atemp, y=temp)) +
  geom_point()

eigen <- cov(temps_recenter) %>% eigen()
eigen_vals <- eigen$values
Gamma <- eigen$vectors

Y <- as.matrix(temps_recenter) %*% Gamma %>%
  as_data_frame() %>%
  rename(Y1 = V1, Y2 = V2)

ggplot(Y, aes(x=Y1, y=Y2)) +
  geom_point()

test <- test %>%
  mutate(temps = Y$Y1)

#------- CREATE MODEL FORMULA  ---------------------------------------------------------

# Create model formula for ridge regression
model_formula <- train %>%
  # Take all predictor variable names and separate them with + signs:
  names() %>%
  setdiff(c("count", "registered", "casual", "datetime", "atemp", "temp")) %>%
  stringr::str_c(collapse=" + ") %>%
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("count ~ ", .)
model_formula <- as.formula(model_formula)

#Create X and Y values: x is every variable in each row and y is the count of every row
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$count

# Number of folds to use for all CV
#Use 10-fold CV for all CV
n_folds <- 10

#------- RIDGE REGRESSION -----------------------------------------------------------------

# Find the optimal lambda from these values. Feel free to toy with the from, to,
# and length values.
lambda_values <- 10^seq(from = -5, to = 10, length = 2500)
#lambda values from 100 to 10,000
lambda_values
max(lambda_values)

# FILL IN BELOW:
model_ridge <- glmnet(X, y, family = "poisson", alpha = 0, lambda = lambda_values)

# Explore using base R tools
coef(model_ridge)
plot(model_ridge, xvar = "lambda", label = TRUE)

#Clean up with broom!
# The coefficients: For each of the 100 values of lambda, the 9 coefficients
model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio))

# Cross-Validation --------------------------------------------------------
cvfit <- cv.glmnet(X, y, family = "poisson", alpha = 0, lambda=lambda_values, nfolds = 10)
cvfit
# Let's plot:
plot(cvfit)

# Optimal lambda from cross-validation!
cvfit %>%
  glance()
lambda_star_ridge <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_ridge

# Gross! broom to the rescue!
cv_results <- cvfit %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))
cv_results

#plot of cv results, display optimal lambda with vertial line
ggplot(cv_results, aes(x=log_lambda)) +
  geom_point(aes(y=estimate)) +
  labs(x="log(lambda)", y="Cross-Validated MSE") +
  # Optional: Errorbars on cross-validated MSE's
  # geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +
  geom_vline(xintercept = log(lambda_star_ridge), col="red")


# Plot!
ridge_coefficients <- model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept = log(lambda_star_ridge), col="red") +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Ridge Regression Coefficients")


# Still a bit hard to read because of multiple colors. Let's make this interactive:
ggplotly()


#------- ROOT MEAN SQUARE LOGERITHMIC ERROR ANALYSIS FOR RIDGE REGRESSION -------------------------
# Create 10 folds
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# For all n_folds folds, save 2 RMSLE's from the 2 methods here. For each fold,
# we will append/bind the results for each fold.
results_all_folds <- NULL

for(i in 1:n_folds){

  # 1. Create disjoint pseudo-train and pseudo-test sets based on folding scheme
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)

  # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
  #Create X and Y values: x is every variable in each row and y is the sale price of every row
  X_new <- model.matrix(model_formula, data = pseudo_train)[,-1]
  X_test <- model.matrix(model_formula, data = pseudo_test)[,-1]
  y <- pseudo_train$count

  model_ridge <- glmnet(X_new, y, family = "poisson", alpha = 0, lambda = lambda_star_ridge)

  # Get predictions for two methods.
  ridge_predictions <- model_ridge %>%
    predict(newx = X_test)

  # Compute RMSLE for each method and save them (by appending/binding to
  # results_all_folds)
  results_all_folds <-
    # Create data frame of y=count and predictions from all three methods
    data_frame(
      count = pseudo_test$count,
      ridge = as.vector(exp(ridge_predictions))
    ) %>%
    summarise(rmsle_ridge = rmsle(ridge, count)) %>%
    mutate(fold=i) %>%
    bind_rows(results_all_folds)

}

results_all_folds %>%
  summarise(RMSLE_ridge = mean(rmsle_ridge))

#------- MAKE PREDICTIONS ---------------------------------------------------------------------

#Dummy count so that model.matrix will still work
test <- test %>%
  mutate(count = 5)

X_test <- model.matrix(model_formula, data = test)[,-1]

# Get predictions using ridge
predictions <- model_ridge %>%
  predict(newx = X_test) %>%
  exp()

#Select proper columns for submission
test <- test %>%
  mutate(count = predictions) %>%
  select(datetime, count)


# Write submissions to CSV
sample_submission <- test %>%
  write_csv("submission.csv")



#------- EXTRAS ---------------------------------------------------------------------------------

#More Exploratory Data Analysis

#Holiday
ggplot(train, aes(x=holiday, y=count)) +
  geom_bar(stat = "identity")

#Temperature
ggplot(train, aes(x=temp, y=count)) +
  geom_point()

#"Feels like" temperature
ggplot(train, aes(x=atemp, y=count)) +
  geom_point()

#Humidity
ggplot(train, aes(x=humidity, y=count)) +
  geom_point()

#Windspeed
ggplot(train, aes(x=windspeed, y=count)) +
  geom_point()



# LASSO MODEL
#Create X and Y values: x is every variable in each row and y is the count of every row
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$count

# Number of folds to use for all CV
#Use 10-fold CV for all CV
n_folds <- 10

lambda_values <- 10^seq(from = -5, to = 10, length = 2500)

model_LASSO <- glmnet(X, y, family = "poisson", alpha = 1, lambda = lambda_values)

#Explore with Base R
coef(model_LASSO)
plot(model_LASSO, xvar = "lambda", label = TRUE)


#Clean up with broom!
# The coefficients: For each of the 100 values of lambda, the 9 coefficients
model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio))

# Plot!
model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()

# Cross-Validation --------------------------------------------------------
cvfit <- cv.glmnet(X, y, family = "poisson", alpha = 1, lambda=lambda_values, nfolds = 10)

# Let's plot:
plot(cvfit)

# Optimal lambda from cross-validation!
cvfit %>%
  glance()
lambda_star_LASSO <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_LASSO

# Gross! broom to the rescue!
cv_results <- cvfit %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))
cv_results

#plot of cv results, display optimal lambda with vertial line
ggplot(cv_results, aes(x=log_lambda)) +
  geom_point(aes(y=estimate)) +
  labs(x="log(lambda)", y="Cross-Validated MSE") +
  # Optional: Errorbars on cross-validated MSE's
  # geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +
  geom_vline(xintercept = log(lambda_star_LASSO), col="red")

# Plot!
LASSO_coefficients <- model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept = log(lambda_star_LASSO), col="red") +
  labs(x="log(lambda)", y="Coefficient Estimate", title="LASSO Regression Coefficients")

# Still a bit hard to read because of multiple colors. Let's make this interactive:
ggplotly()

# ROOT MEAN SQUARE LOGERITHMIC ERROR ANALYSIS FOR LASSO
# Create 10 folds
# train <- train %>%
#   sample_frac(1) %>%
#   mutate(fold = rep(1:n_folds, length=n())) %>%
#   arrange(fold)
#
# View(train)
# For all n_folds folds, save 2 RMSLE's from the 2 methods here. For each fold,
# we will append/bind the results for each fold.
results_all_folds <- NULL

for(i in 1:n_folds){

  # 1. Create disjoint pseudo-train and pseudo-test sets based on folding scheme
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)

  # 2. Train the model using (i.e. fit the model to) the pseudo_train data.
  #Create X and Y values: x is every variable in each row and y is the sale price of every row
  X_new <- model.matrix(model_formula, data = pseudo_train)[,-1]
  X_test <- model.matrix(model_formula, data = pseudo_test)[,-1]
  y <- pseudo_train$count

  model_LASSO <- glmnet(X_new, y, family = "poisson", alpha = 1, lambda = lambda_star_LASSO)

  # Get predictions for LASSO.
  LASSO_predictions <- model_LASSO %>%
    predict(newx = X_test)

  # Compute RMSLE for each method and save them (by appending/binding to
  # results_all_folds)
  results_all_folds <-
    # Create data frame of y=count and predictions from all three methods
    data_frame(
      count = pseudo_test$count,
      LASSO = as.vector(exp(LASSO_predictions))
    ) %>%
    summarise(rmsle_LASSO = rmsle(LASSO, count)) %>%
    mutate(fold=i) %>%
    bind_rows(results_all_folds)

}

results_all_folds %>%
  summarise(RMSLE_LASSO=mean(rmsle_LASSO))










