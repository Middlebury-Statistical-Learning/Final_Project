#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Sierra Moen
# Kaggle Competition Name: Give me some credit
# Kaggle Competition URL: https://www.kaggle.com/c/GiveMeSomeCredit
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(broom)
library(ROCR)
library(readr)
library(dplyr)
library(plotly)
library(glmnet)
library(stringr)


# 2. Load Data Files & Data Cleaning --------------------------------------
train_1 <- read_csv("Files/cs-training.csv")
test_1 <- read_csv("Files/cs-test.csv")

train <-
  dplyr::rename(train_1,
                Times30_59DaysPastDue = `NumberOfTime30-59DaysPastDueNotWorse`,
                Times60_89DaysPastDue = `NumberOfTime60-89DaysPastDueNotWorse`,
                Open_Credit_Lines = `NumberOfOpenCreditLinesAndLoans`,
                Times_90_Days_Late = `NumberOfTimes90DaysLate`,
                Real_Estate_Loans = `NumberRealEstateLoansOrLines`,
                Dependents = `NumberOfDependents`,
                Debt_Ratio = `DebtRatio`
  ) %>%
  mutate(
    Debt_Ratio = ifelse(is.na(Debt_Ratio ), mean(Debt_Ratio, na.rm=TRUE), Debt_Ratio)
  ) %>%
  mutate(
    MonthlyIncome = ifelse(is.na(MonthlyIncome ), mean(MonthlyIncome, na.rm=TRUE), MonthlyIncome)
  ) %>%
  mutate(
    Dependents = ifelse(is.na(Dependents ), mean(Dependents, na.rm=TRUE), Dependents)
  )

y <- is.na(train) # returns TRUE if x is missing

test <-
  dplyr::rename(test_1,
                Times30_59DaysPastDue = `NumberOfTime30-59DaysPastDueNotWorse`,
                Times60_89DaysPastDue = `NumberOfTime60-89DaysPastDueNotWorse`,
                Open_Credit_Lines = `NumberOfOpenCreditLinesAndLoans`,
                Times_90_Days_Late = `NumberOfTimes90DaysLate`,
                Real_Estate_Loans = `NumberRealEstateLoansOrLines`,
                Dependents = `NumberOfDependents`,
                Debt_Ratio = `DebtRatio`
  ) %>%
  mutate(
    Debt_Ratio = ifelse(is.na(Debt_Ratio ), mean(Debt_Ratio, na.rm=TRUE), Debt_Ratio)
  ) %>%
  mutate(
    MonthlyIncome = ifelse(is.na(MonthlyIncome ), mean(MonthlyIncome, na.rm=TRUE), MonthlyIncome)
  ) %>%
  mutate(
    Dependents = ifelse(is.na(Dependents ), mean(Dependents, na.rm=TRUE), Dependents)
  )

y <- is.na(train) # returns TRUE if x is missing


# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------
ggplot(data = train, aes(x = factor(SeriousDlqin2yrs), y = age)) +
  geom_boxplot()
#age isn't bad as a predictor using this analysis - younger people are significantly more likely to experience distress

train4 <- filter(train, Times60_89DaysPastDue != "98", Times60_89DaysPastDue != "96")
ggplot(data = train4, aes(x = factor(SeriousDlqin2yrs), y = Times60_89DaysPastDue)) +
  geom_boxplot()

train2 <- filter(train, Times_90_Days_Late != "98", Times_90_Days_Late != "96")
ggplot(data = train2, aes(x = factor(SeriousDlqin2yrs), y = Times_90_Days_Late)) +
  geom_boxplot()

ggplot(data = train, aes(x = factor(SeriousDlqin2yrs), y = Debt_Ratio)) +
  geom_boxplot()

ggplot(data = train, aes(x = factor(SeriousDlqin2yrs), y = Open_Credit_Lines)) +
  geom_boxplot()

#train %>% dplyr::select(X1, SeriousDlqin2yrs, age, RevolvingUtilizationOfUnsecuredLines, Times30_59DaysPastDue, Times60_89DaysPastDue, Times_90_Days_Late, Debt_Ratio)


# 4. Cross-Validation of Final Model --------------------------------------
# Training Logistic Model and Making Predictions --------------------
train_orig <- train

train <- train_orig %>%
  sample_frac(0.5)

test <- train_orig %>%
  anti_join(train, by="X1")


# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command

model_formula <- as.formula(SeriousDlqin2yrs ~ Times30_59DaysPastDue + Times60_89DaysPastDue + age + RevolvingUtilizationOfUnsecuredLines + Times_90_Days_Late + Debt_Ratio)
model_logistic <- glm(model_formula, data=train, family="binomial")


# 1. Predictions using predict() ---------------------------------------------
# predict() for logistic regression returns fitted values in log-odds space
# i.e. values from (-infty, infty)
predict(model_logistic, newdata=test_real) %>% round(3)

# Recall from class notes that we fit the regression in log-odds space first,
# then convert back to probability space [0,1] using the inverse-logit function.
# These values below are now probabilities!
p_hat <- 1/(1+exp(-predict(model_logistic, newdata = test_real)))
p_hat <- round(p_hat, 3)
p_hat

# You can do this automatically by setting type="response". Note how these two
# sets of values are identical:
t9 <- predict(model_logistic, newdata = test_real, type="response") %>% round(3)

test_real <- test_real %>%
  mutate(Probability=t9) %>%
  mutate(Id=X1) %>%
  dplyr::select(Id, Probability)

# 5. Create Submission ----------------------------------------------------

test_real %>% write_csv("predictions48.csv")


#Score is 0.695533



# 6. Extras ---------------------------------------------------------------

# Section A: More EDA

ggplot(data = train, aes(x = factor(SeriousDlqin2yrs), y = RevolvingUtilizationOfUnsecuredLines)) +
  geom_line()
# this looks good! I used a line graph, which is weird since I'm not plotting something over time, but it shows a BIG difference based on this variable!

train3 <- filter(train, Times30_59DaysPastDue != "98", Times30_59DaysPastDue != "96")
ggplot(data = train3, aes(x = factor(SeriousDlqin2yrs), y = Times30_59DaysPastDue)) +
  geom_boxplot()

ggplot(data = train, aes(x = factor(SeriousDlqin2yrs), y = Real_Estate_Loans)) +
  geom_boxplot()

ggplot(data = train, aes(x = factor(SeriousDlqin2yrs), y = Dependents)) +
  geom_boxplot()
#I wonder if this is significant. Less spread for 1 but larger 3rd quartile




# Section B

# Ridge Regression & LASSO Analysis (starter code was copied from PS06)

# Create the model formula for lm(), ridge regression, and LASSO:
model_formula <- train %>%
  names() %>%
  setdiff(c("Id", "fold", "SeriousDlqin2yrs")) %>%
  stringr::str_c(collapse=" + ") %>%
  stringr::str_c("SeriousDlqin2yrs ~ ", .)
model_formula
model_formula <- as.formula(model_formula)

# Used in glmnet() function for ridge regression and LASSO
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$SeriousDlqin2yrs

# Number of folds to use for all CV
n_folds <- 10 # this seems arbitrary, maybe check back in FUTURE

# 1. Obtain optimal lambda for ridge regression --------------------------------

lambda_values <- 10^seq(from = -20, to = 10, length = 1000)

cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)
cv_ridge_tidy <- cv_ridge %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))

ggplot(cv_ridge_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="Ridge Regression Cross-Validation")

# Extract optimal lambda using broom::glance()
lambda_star_ridge <- cv_ridge %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_ridge

# Show same plot but with optimal lambda marked in red
ggplot(cv_ridge_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="Ridge Regression Cross-Validation") +
  geom_vline(xintercept = log(lambda_star_ridge), col="red")

# Compute coefficients for each value of lambda
lambda_values <- 10^seq(from = -15, to = 10, length = 2500)
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)
coefficients <- model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)")

# Plot coefficients for each value of lambda, with optimal lambda marked
ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_ridge)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="Ridge Regression Coefficients")


# 2. Obtain optimal lambda for LASSO -------------------------------------------

lambda_values <- 10^seq(from = -15, to = 10, length = 2500)
cv_LASSO <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)

# Plot cross-validated MSE value for each lambda:
cv_LASSO_tidy <- cv_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))

# Extract optimal lambda using broom::glance()
lambda_star_LASSO <- cv_LASSO %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_LASSO

# Show same plot but with optimal lambda marked in red
ggplot(cv_LASSO_tidy, aes(x=log_lambda, y=estimate)) +
  geom_point() +
  labs(x="log(lambda)", y="Cross-Validated MSE", title="LASSO Cross-Validation") +
  geom_vline(xintercept = log(lambda_star_LASSO), col="red")

# # Compute coefficients for each value of lambda
lambda_values <- 10^seq(from = -15, to = 10, length = 5000)
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_values)
coefficients <- model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)")

# Plot coefficients for each value of lambda, with optimal lambda marked
ggplot(coefficients, aes(x=log_lambda, y=estimate, col=term)) +
  geom_line() +
  geom_vline(xintercept=log(lambda_star_LASSO)) +
  labs(x="log(lambda)", y="Coefficient Estimate", title="LASSO Coefficients")


# 3. Cross-validation comparison of lm(), ridge, and LASSO ---------------------
# Create folds
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# For all n_folds folds, save 3 MSE's from the 3 methods here. For each fold
# we will append/bind the results for each fold.
results_all_folds <- NULL

for(i in 1:n_folds){
  # 1. Create pseudo train/test based on folding scheme
  pseudo_train <- train %>%
    filter(fold != i)
  pseudo_test <- train %>%
    filter(fold == i)
  # Needed for glmnet() methods:
  pseudo_train_X <- model.matrix(model_formula, data = pseudo_train)[, -1]
  pseudo_train_y <- pseudo_train$SeriousDlqin2yrs
  pseudo_test_X <- model.matrix(model_formula, data = pseudo_test)[, -1]

  # 2. Fit model to pseudo-train
  model_lm <- lm(model_formula, data=pseudo_train)
  model_ridge <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 0, lambda = lambda_star_ridge)
  model_LASSO <- glmnet(pseudo_train_X, pseudo_train_y, alpha = 1, lambda = lambda_star_LASSO)

  # 3. Get predictions for all three methods.
  lm_predictions <- model_lm %>%
    predict(newdata=pseudo_test)
  ridge_predictions <- model_ridge %>%
    predict(newx=pseudo_test_X, s=lambda_star_ridge)
  LASSO_predictions <- model_LASSO %>%
    predict(newx=pseudo_test_X, s=lambda_star_LASSO)

  # 4. Compute MSE for each method and save them (by appending/binding to
  # results_all_folds)
  results_all_folds <-
    # Create data frame of y=SeriousDlqin2yrs and predictions from all three methods
    data_frame(
      SeriousDlqin2yrs = pseudo_test$SeriousDlqin2yrs,
      lm = as.vector(lm_predictions),
      ridge = as.vector(ridge_predictions),
      LASSO = as.vector(LASSO_predictions)
    ) %>%
    # Switch to tidy format so we can use group_by() to compute MSE's for all
    # three methods easily
    tidyr::gather(method, yhat, -SeriousDlqin2yrs) %>%
    group_by(method) %>%
    summarise(MSE = mean((SeriousDlqin2yrs-yhat)^2)) %>%
    mutate(fold=i) %>%
    bind_rows(results_all_folds)
}

# Show results
results_all_folds %>%
  group_by(method) %>%
  summarise(MSE_CV = mean(MSE)) %>%
  arrange(desc(MSE_CV))

# Run the CV comparision a few times manually (in order to account for sampling
# variability) and get a rough ranking of which method does best. You will use
# this method below

test_X <- model.matrix(model_formula, data = test)[, -1]
# test_X isn't working because it says it couldn't find many things. I reran the whole code without selecting things so don't really know what else to do.


# lm() returned the best results for me, but I am hesitant to believe that, as it is the simplest model.


# In my case, lm had the best MSE, so let's use it with the
# optimal lambda value:

predictions <- model_lm %>%
  predict(newdata=test)

#train %>% dplyr::select(X1, SeriousDlqin2yrs, age, RevolvingUtilizationOfUnsecuredLines, Times30_59DaysPastDue, Times60_89DaysPastDue, Times_90_Days_Late, Debt_Ratio)
model_formula <- as.formula(SeriousDlqin2yrs ~ Times30_59DaysPastDue + Times60_89DaysPastDue + age + RevolvingUtilizationOfUnsecuredLines + Times_90_Days_Late + Debt_Ratio)
model_logistic <- glm(model_formula, data=train, family="binomial")

model <- glm(model_formula, data=train, SeriousDlqin2yrs ~ Times30_59DaysPastDue + Times60_89DaysPastDue + age + RevolvingUtilizationOfUnsecuredLines + Times_90_Days_Late + Debt_Ratio, data = train)
train_data_augmented <- model %>%
  augment() %>%
  tbl_df()
# Plot fitted model to training data
ggplot(data=train_data_augmented, aes(x=Times30_59DaysPastDue)) +
  geom_point(aes(y=SeriousDlqin2yrs)) +
  geom_line(aes(y=.fitted), col="blue")

# 3. Get Predictions

predictions <- model_logistic %>%
  augment(newdata=test) %>%
  tbl_df() %>%
  .[[".fitted"]] %>%
  as.data.frame()

t3 <- cbind(predictions, test) %>% filter(predictions < 1, predictions > -1)
t4 <- abs(predictions) %>% cbind(predictions, test)

View(t4)
t4 %>%

  probs_reorder <- t3 %>% select("X1", "predictions")




pseudo_train <- train %>%
  anti_join(pseudo_test, by="fold")

t3 %>% write_csv("predictions45.csv")



profiles_train <- profiles %>%
  sample_frac(0.5)

profiles_test <- profiles %>%
  anti_join(profiles_train, by="ID")

# Train logistic regression. Note the glm(..., family="binomial") and not lm()
# command
model_formula <- as.formula(y~height)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")

# Recall there have been generally two ways in this class to obtain predictions
# based on a model: using predict(), or using augment(). Let's look at the first
# 20:





# 1. Predictions using predict() ---------------------------------------------
# predict() for logistic regression returns fitted values in log-odds space
# i.e. values from (-infty, infty)
predict(model_logistic, newdata = profiles_test)[1:10] %>% round(3)

# Recall from class notes that we fit the regression in log-odds space first,
# then convert back to probability space [0,1] using the inverse-logit function.
# These values below are now probabilities!
p_hat <- 1/(1+exp(-predict(model_logistic, newdata = profiles_test)[1:10]))
p_hat <- round(p_hat, 3)
p_hat

# You can do this automatically by setting type="response". Note how these two
# sets of values are identical:
predict(model_logistic, newdata = profiles_test, type="response")[1:10] %>% round(3)
p_hat





# 2. Predictions using augment() ---------------------------------------------
# The .fitted column below is the same as the output of predict(model_logistic)
# above: in log-odds space (-infty, infty)
model_logistic %>%
  augment(newdata=profiles_test) %>%
  tbl_df()

# Again we can convert the fitted values to probabilities p_hat by using the
# inverse logit function:
model_logistic %>%
  augment(newdata=profiles_test) %>%
  tbl_df() %>%
  mutate(
    p_hat = 1/(1+exp(-.fitted)),
    p_hat = round(p_hat, 3)
  )



# Section C: ROC curves
# this used to work and now it doesn't! That is frusterating!
train_ROC <- train_ROC %>%
  mutate(
    p_hat = predict(model_logistic, type="response"),
    p_hat = round(p_hat, 3)
  ) %>%
  select(y, p_hat)
View(train_ROC)


p_star_values <- seq(.001, .999, 0.001)
p_star_values


costs <- rep(0, length(p_star_values))

for(j in 1:length(p_star_values)){

  p2 <- train_ROC %>% mutate(y_hat =ifelse(p_hat>p_star_values[j], 1, 0)) %>%
    mutate(cost=0) %>%
    mutate(cost =ifelse(y==1 & y_hat==0, 2, cost)) %>%
    mutate(cost =ifelse(y==0 & y_hat==1, 1, cost))

  p3 <- as.numeric(summarize(p2, mean = mean(cost, na.rm = TRUE)))

  costs[j]<-p3
}

results <- data_frame(
  Cost = costs,
  p_star = p_star_values)



