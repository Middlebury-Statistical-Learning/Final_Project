##########################################################
# MATH 218 FINAL PROJECT - Bike Sharing Demand
##########################################################
# link to kaggle competition: https://www.kaggle.com/c/bike-sharing-demand


#Section I - Load Packages -------------------------------------------------
library(tidyverse)
library(rpart)
library(broom)
library(glmnet)
library(msaenet) #package for RMSLE

#Section II - Data Cleaning ----------------------------------------------------------------
train <- read_csv("Files/train.csv")
test <- read_csv("Files/test.csv")

#generate a control for "later" observations
train <- train %>%
  mutate(late = datetime>"2012-01-01")
test <- test %>%
  mutate(late = datetime>"2012-01-01")

#day of week variable, inspired by http://brandonharris.io/kaggle-bike-sharing/
train$day <- weekdays(as.Date(train$datetime))
train$day <- as.factor(train$day)
test$day <- weekdays(as.Date(test$datetime))
test$day <- as.factor(test$day)

# Time of day variable, inspired by http://brandonharris.io/kaggle-bike-sharing/
#create time column by stripping out timestamp
train$time <- substring(train$datetime,12,20)
test$time <- substring(test$datetime,12,20)
#factorize new timestamp column
train$time <- factor(train$time)
test$time <- factor(test$time)
#convert time and create $hour as integer to evaluate for daypart
train$hour<- as.numeric(substr(train$time,1,2))
test$hour<- as.numeric(substr(test$time,1,2))
#create daypart column, default to 4 to make things easier for ourselves
train$daypart <- "4"
test$daypart <- "4"
#4AM - 10AM = 1
train$daypart[(train$hour < 10) & (train$hour > 3)] <- 1
test$daypart[(test$hour < 10) & (test$hour > 3)] <- 1
#11AM - 3PM = 2
train$daypart[(train$hour < 16) & (train$hour > 9)] <- 2
test$daypart[(test$hour < 16) & (test$hour > 9)] <- 2
#4PM - 9PM = 3
train$daypart[(train$hour < 22) & (train$hour > 15)] <- 3
test$daypart[(test$hour < 22) & (test$hour > 15)] <- 3
#convert daypart to factor
train$daypart <- as.factor(train$daypart)
test$daypart <- as.factor(test$daypart)
#convert hour back to factor
train$hour <- as.factor(train$hour)
test$hour <- as.factor(test$hour)


#dummy for sunday
train <- train %>%
  mutate(sunday=ifelse(day== "Sunday",1,0))
test <- test %>%
  mutate(sunday=ifelse(day== "Sunday",1,0))

#Test set doesn't have extreme weather, and so it was messing up my fixed effects - deleting it.
train <- train %>%
  mutate(weather=replace(weather, weather==4, 3))
test <- test %>%
  mutate(weather=replace(weather, weather==4, 3))


#add some labels
train$season <- factor(train$season, labels = c("Spring", "Summer", "Fall", "Winter"))
train$weather <- factor(train$weather, labels = c("Clear", "Mist", "Light Rain/Snow"))
test$season <- factor(test$season, labels = c("Spring", "Summer", "Fall", "Winter"))
test$weather <- factor(test$weather, labels = c("Clear", "Mist", "Light Rain/Snow"))





##########################################################
# SECTION III - EXPLORATORY DATA ANALYSIS -----------------------------------------------------------------------
##########################################################

########## RENTALS BY TIME
train%>%
  ggplot(aes(x=datetime, y=count, color=late)) +
  geom_point(shape=1, alpha=.1) +
  geom_smooth() +
  labs(x="Date", y="# of Total Rentals", title="The evolution of bike rentals through time")


### Plotting 'feels like' temperature and rentals
# positive relationship, until it gets "too hot"
train %>%
  ggplot(aes(x=atemp, y=count)) +
  geom_point(shape=1, alpha=.1, position = "jitter") +
  geom_smooth() +
  labs(x="'feels like' Temperature (°C)", y="# of Total Rentals", title="Relationship between temperature and demand")
#theme(axis.title=element_text(size=13), title = element_text(size=16))


### Plotting time of day and rentals
train %>%
  ggplot(aes(x=daypart, y=count)) +
  geom_boxplot() +
  labs(x="Time period", y="# of Total Rentals", title="Relationship between time of day and demand")
#afternoon is a good time!

### Rentals by Season (probably correlated with temperature)
train %>%
  ggplot(aes(x=factor(season), y=count)) +
  geom_boxplot() +
  labs(x="Season", y="# of Total Rentals", title="Rental averages by season")
#Fall is the best season apparently!


############################################################
# MAIN SUBMISSION - CV for Regression Tree ----------------------------------------------------------------------
###########################################################
# Set up model
model_formula <- as.formula("count~season+holiday+workingday+weather+windspeed+atemp+late+humidity+day+sunday+daypart+time")
depth_vector <- 3:20      #different tree depths
# Number of folds for x-validation
n_folds <- 10

#Assign fold numbers
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

#store scores here
fold_RMSLE_tree <- rep(0, length(n_folds))
depth_RMSLE <- rep(0, length(depth_vector))

#outer loop - testing different depths
for(j in 1:length(depth_vector)) {

  #inner loop
  for (i in 1:n_folds) {
    pseudo_test <- train %>%
      filter(fold==i)
    pseudo_train <- train %>%
      filter(fold!=i)

    #fit tree
    knobs <- rpart.control(maxdepth = depth_vector[j])
    model_CART <- rpart(model_formula, data = train, control=knobs)

    #predictions
    tree_predictions <- predict(model_CART, newdata=pseudo_test)




    #store RMSLE for this fold
    fold_RMSLE_tree[i] <- msaenet.rmsle(pseudo_test$count, tree_predictions)
  }

  depth_RMSLE[j] <- mean(fold_RMSLE_tree)
  if(j %% 5 == 0){
    paste("Done CV for depth =", depth_vector[j], "on", Sys.time()) %>% print()}
}

# Let's study the output:
print(model_CART)
summary(model_CART)
plot(model_CART, margin=0.1)
text(model_CART, use.n = TRUE)
box()

#Find optimal max depth and make predictions with it
rmsle_min_tree <- min(depth_RMSLE)
depth_report <- data_frame(depth_vector, depth_RMSLE)
optimal_depth <- depth_report %>%
  filter(depth_RMSLE==min(depth_RMSLE)) %>%
  select(depth_vector)
optimal_depth <- min(optimal_depth$depth_vector)


model_CART <- rpart(model_formula, data = train, control=optimal_depth)
CART_predictions <- predict(model_CART, newdata = test)
kaggle_CART <- data_frame(test$datetime, CART_predictions)

#Write submissions to CSV
names(kaggle_CART) <- c("datetime", "count")
kaggle_CART %>%
  write_csv("Files/CART_submission.csv")

#The submission added random strings where there should be spaces in "datetime".
#Nothing in the R environment shows them, but once I open the csv, the datetime variable has these awkward strings, which I had to remove before submitting."
# I think it's some problem with how R outputs datetime variables as csv














# And that's all, folks!
# Final Kaggle score for CART: 0.85008
# Final Kaggle score for LASSO (see appendix): 1.11049


############################################################
# APPENDIX
############################################################

#Less interesting plots#
#holidays
train %>%
  ggplot(aes(x=factor(holiday), y=count)) +
  geom_boxplot() +
  labs(x="1 = Holiday", y="# of Total Rentals", title="Is there a different # of rentals on holidays?")

### Rentals by weather condition
train %>%
  ggplot(aes(x=factor(weather), y=count)) +
  geom_boxplot() +
  labs(x="Weather Conditions", y="# of Total Rentals", title="Rental averages by season")


#working days
train %>%
  ggplot(aes(x=factor(workingday), y=count)) +
  geom_boxplot(aes(ymax=500)) +
  labs(x="1 = Working Day", y="# of Total Rentals", title="Is there a different # of rentals on working days?")


#humidity and demand#
train %>%
  ggplot(aes(x=humidity, y=count)) +
  geom_point(shape=1, alpha=.1, position = "jitter") +
  geom_smooth() +
  labs(x="Relative Humidity", y="# of Total Rentals", title="Relationship between humidity and demand")

#wind speed and demand#
train %>%
  ggplot(aes(x=windspeed, y=count)) +
  geom_point(shape=1, alpha=.1, position = "jitter") +
  geom_smooth() +
  labs(x="Wind Speed", y="# of Total Rentals", title="Relationship between wind speed and demand")


### I imagine temp and atemp are highly correlated, use PCA?
train %>%
  ggplot(aes(x=temp, y=atemp)) +
  geom_point(shape=1, alpha=.1, position = "jitter") +
  geom_smooth(method="lm", se=FALSE, linetype="dashed", size=0.5, col="black")
# as you can see they overlap a lot (even with jitter)



##
# I do PCA for temp and atemp, but end up not using that in my models
###
X <- train %>%
  select(atemp,temp)

#Recentering variables
X_recenter <- X %>%
  mutate(
    atemp = atemp - mean(atemp),
    temp = temp - mean(temp)
  )
#Covariance Matrix
cov(X) %>% round(3)
eigen <- cov(X_recenter) %>% eigen()
eigen_vals <- eigen$values
Gamma <- eigen$vectors

# Transform n x p matrix of observations to principal components space using
# matrix multiplication operator %*%:
Y <- as.matrix(X_recenter) %*% Gamma %>%
  as_data_frame() %>%
  rename(Y1 = V1, Y2 = V2)

var_Y <- cov(Y) %>% diag()
var_Y
cumsum(var_Y)/sum(var_Y)
# Over 99% of the variability in temperature and feels like is explained by the first
#component!

#Adding Y1 to train data
train <- train %>%
  mutate(temp_pca=Y$Y1)
#Adding Y1 to test data
X_test <- test %>%
  select(atemp,temp) %>%
  mutate(
    atemp = atemp - mean(atemp),
    temp = temp - mean(temp)
  )
Gamma_test <- eigen$vectors
Y_test <- as.matrix(X_test) %*% Gamma_test %>%
  as_data_frame() %>%
  rename(Y1 = V1, Y2 = V2)
test <- test %>%
  mutate(temp_pca=Y_test$Y1)

#END of PCA



########### OTHER THINGS I TRIED ######################################################################


#############################################################
#CV for LASSO using RMSLE as the scoring mechanism -------------------------------------
#############################################################
# Set up model
model_formula <- as.formula("count~factor(season)+holiday+workingday+factor(weather)+windspeed+atemp+late+humidity+factor(day)+daypart+time")

# Vector of different lambdas to try
lambda_vector <- 0:150      #not doing it on a log scale because I want to check lambdas of 0 (that is, a simple lm)
c
#Store RMSLEs here
fold_RMSLE <- rep(0, length(n_folds))
lambda_RMSLE <- rep(0, length(lambda_vector))

#outer loop - testing different lambdas
for(j in 1:length(lambda_vector)) {

  #inner loop
  for (i in 1:n_folds) {
    pseudo_test <- train %>%
      filter(fold==i)
    pseudo_train <- train %>%
      filter(fold!=i)


    #fit model to pseudo_train
    # Used in glmnet() function for ridge regression and LASSO
    X <- model.matrix(model_formula, data = pseudo_train)[, -1]
    y <- pseudo_train$count

    model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_vector[j])

    #predict values in pseudo_test
    pseudo_test_X <- model.matrix(model_formula, data = pseudo_test)[, -1]
    LASSO_predictions <- model_LASSO %>%
      predict(newx=pseudo_test_X, s=lambda_vector[j])
    # Changing my predicted negative values to 0, can't go wrong with that
    LASSO_predictions <- replace(LASSO_predictions, LASSO_predictions<0, 0)



    #store RMSLE for this fold
    fold_RMSLE[i] <- msaenet.rmsle(pseudo_test$count, LASSO_predictions)

  }
  #average all folds for given lambda
  lambda_RMSLE[j] <- mean(fold_RMSLE)
  if(j %% 25 == 0){
    paste("Done CV for lambda =", lambda_vector[j], "on", Sys.time()) %>% print()}

}


lambda_frame <- data_frame(lambda_vector, lambda_RMSLE)

ggplot(lambda_frame, aes(x=lambda_vector, y=lambda_RMSLE)) +
  geom_point() +
  labs(x="Lambda", y="Cross-Validated RLMSE", title="LASSO Cross-Validation")

#Find minimum RMSLE value
rmsle_min_LASSO <- min(lambda_frame$lambda_RMSLE)
lambda_star_lasso <- lambda_frame %>%
  filter(lambda_RMSLE==min(lambda_RMSLE)) %>%
  select(lambda_vector)
lambda_star_lasso <- max(lambda_star_lasso$lambda_vector)


#Plotting how coefficients shrink as lambda increases
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$count
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_vector)
coefficients <- model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)")

# Plot coefficients for each value of lambda,
ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  labs(x="log(lambda)", y="Coefficient Estimate", title="LASSO Coefficients")

#Fit predictions using test data
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_star_lasso)
test_X <- model.matrix(as.formula("~factor(season)+holiday+workingday+factor(weather)+windspeed+atemp+late+humidity+factor(day)+daypart+time"), data = test)[, -1]
#test_X <- model.matrix(model_formula, data = test)[, -1] for some reason this doesn't work
LASSO_submission <- model_LASSO %>%
  predict(newx=test_X, s=lambda_star_lasso)
LASSO_submission <- replace(LASSO_submission, LASSO_submission<0, 0)
LASSO_submission<- as.vector(LASSO_submission)


kaggle_LASSO <- data_frame(test$datetime, LASSO_submission)
names(kaggle_LASSO) <- c("datetime", "count")
# Write submissions to CSV
kaggle_LASSO %>%
  write_csv("LASSO_submission.csv")

test$datetime %>%
  write.csv("datetime.csv")

##################################################################
#CV for Linear regression using RMSLE as the scoring mechanism ------------------------------
##################################################################
n_folds <- 10
fold_RMSLE_lm <- rep(0, length(n_folds))
#loop
for (i in 1:n_folds) {
  pseudo_test <- train %>%
    filter(fold==i)
  pseudo_train <- train %>%
    filter(fold!=i)

  #fit model to pseudo_train
  model_lm <- lm(model_formula, data=pseudo_train)



  #predict values in pseudo_test
  lm_predictions <- model_lm %>%
    predict(newdata=pseudo_test)
  lm_predictions <- replace(lm_predictions, lm_predictions<0, 0)
  #store RMSLE for this fold
  fold_RMSLE_lm[i] <- msaenet.rmsle(pseudo_test$count, lm_predictions)

}
rmsle_min_lm <- mean(fold_RMSLE_lm)


# Fit predictions to test data (since lambda=0, just doing lm)
model_lm <- lm(model_formula, data=train)

lm_submission <- model_lm %>%
  predict(newdata=test)
kaggle_lm <- data_frame(test$datetime, lm_predictions)
# Changing my predicted negative values to 0, can't go wrong with that
kaggle_lm <- kaggle_lm %>%
  mutate(lm_submission=replace(lm_submission, lm_submission<0, 0))


#############################################################
#CV for Poisson LASSO using RMSLE as the scoring mechanism --------------------------------------
#############################################################
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$count
# Set up model
model_formula <- as.formula("count~factor(season)+holiday+workingday+factor(weather)+windspeed+atemp+late+humidity+day+daypart+time")

# Vector of different lambdas to try
lambda_vector <- 0:150
# Number of folds for x-validation
n_folds <- 10

#Assign fold numbers
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

#Store RMSLEs here
fold_RMSLE_poisson <- rep(0, length(n_folds))
lambda_RMSLE_poisson <- rep(0, length(lambda_vector))

#outer loop - testing different lambdas
for(j in 1:length(lambda_vector)) {

  #inner loop
  for (i in 1:n_folds) {
    pseudo_test <- train %>%
      filter(fold==i)
    pseudo_train <- train %>%
      filter(fold!=i)


    #fit model to pseudo_train
    X <- model.matrix(model_formula, data = pseudo_train)[, -1]
    y <- pseudo_train$count

    model_poisson <- glmnet(X, y, family = "poisson", alpha=1, lambda=lambda_vector[j])

    #predict values in pseudo_test
    pseudo_test_X <- model.matrix(model_formula, data = pseudo_test)[, -1]
    poisson_predictions <- model_poisson %>%
      predict(newx=pseudo_test_X, s=lambda_vector[j])

    #store RMSLE for this fold
    fold_RMSLE_poisson[i] <- msaenet.rmsle(pseudo_test$count, poisson_predictions)

  }
  #average all folds for given lambda
  lambda_RMSLE_poisson[j] <- mean(fold_RMSLE_poisson)
  if(j %% 25 == 0){
    paste("Done CV for lambda =", lambda_vector[j], "on", Sys.time()) %>% print()}

}

#PLOT RMSLE VALUES - LASSO with linear regression works better
lambda_frame_poisson <- data_frame(lambda_vector, lambda_RMSLE_poisson)
lambda_frame_poisson <- lambda_frame_poisson %>%
  mutate(log_lambda=log(lambda_vector))

ggplot(lambda_frame_poisson, aes(x=log_lambda, y=lambda_RMSLE_poisson)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated RLMSE", title="Poisson LASSO Cross-Validation")

#Find minimum RMSLE value
rmsle_min_poisson <- min(lambda_frame_poisson$lambda_RMSLE_poisson)

#this actually scores worse than just predicting the mean

