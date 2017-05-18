#hello and welcome to part 3. In this part I'm going to try lasso vs ridge. 
#i feel good about these variables BUT
# I'm going to try to take out variables to make the model as good as possible while being simple


library(tidyverse)
library(stringr)
library(broom)
library(glmnet)
library(plotly)

#Allstate claims data


#--load data--
train<- read_csv("~/Google Drive/Math/final_train.csv")
test<- read_csv("~/Google Drive/Math/final_test.csv")
sample_sub <- read_csv("~/Google Drive/Math/final_sample_submission.csv")

#create dummy var (useful for lasso & ridge) / pick variables for train2
train1 <- train %>% 
  mutate(dummy_var = 1) %>%
  sample_frac(0.7)
test <- test %>% 
  mutate(dummy_var = 1)

train2<- train1 %>%
  select(id,loss, dummy_var,cat1, cont2, cont3, cont7,  cat7, cat10, cat12, cat23, cat36, cat40, cat28, 
         cat57, cat50, cat41, cat70, cat80, cat72, cat81, cat87, cat100, cat94)

#create model
model_formula <- train2 %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("id", "loss", "dummy_var")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("dummy_var ~ ", .)
model_formula
model_formula <- as.formula(model_formula)

X <- model.matrix(model_formula, data = train2)[, -1]
y <- train2$loss


#RIDGE VS LASSO SHOWDOWN
lambda_values <- 10^seq(from = 2, to = 4, length = 100)

#optimal lambda for ridge
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)
coef(model_ridge)
plot(model_ridge, xvar = "lambda", label = TRUE)

model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio))

# Plot!
model_ridge %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()
ggplotly()
#57 looks big here

# -- optimal lambda -- 
cvfit <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = 10)

plot(cvfit)

# Optimal lambda from cross-validation!
cvfit %>%
  glance()
lambda_star_ridge <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_ridge

#cleaning it up
cv_results <- cvfit %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))
cv_results

ggplot(cv_results, aes(x=log_lambda)) +
  geom_point(aes(y=estimate)) +
  labs(x="log(lambda)", y="Cross-Validated MSE") +
  # Optional: Errorbars on cross-validated MSE's
  #geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +
  geom_vline(xintercept = log(lambda_star_ridge), col="red")
#ok so we have a small lambda... this might mean that a linear model would be best
#can i even have a linear model with categorical

#optimal for lasso
lambda_values <- 10^seq(from = 2, to = 4, length = 100)
model_lasso<- glmnet(X, y, alpha = 1, lambda = lambda_values)
coef(model_lasso)
plot(model_lasso, xvar = "lambda", label = TRUE) 

model_lasso %>%
  tidy() %>%
  tbl_df() %>%
  select(-c(step, dev.ratio))

# Plot!
model_lasso %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()
ggplotly()

# -- optimal lambda -- 
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

plot(cvfit)

# Optimal lambda from cross-validation!
cvfit %>%
  glance()
lambda_star_lasso <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star_lasso

#cleaning it up
cv_results <- cvfit %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))
cv_results

ggplot(cv_results, aes(x=log_lambda)) +
  geom_point(aes(y=estimate)) +
  labs(x="log(lambda)", y="Cross-Validated MSE") +
  # Optional: Errorbars on cross-validated MSE's
  #geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +
  geom_vline(xintercept = log(lambda_star_lasso), col="red")


#time 2 compare!
n_folds <- 5
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# For all n_folds folds, save 3 MSE's from the 3 methods here. For each fold,
# we will append/bind the results for each fold.
results_all_folds <- NULL

for(i in 1:n_folds){
  
  #create folds
  pseudo_train <- train2 %>%
    filter(fold != i)
  pseudo_test <- train2 %>%
    filter(fold == i)
  
  #create pseudo train models
  
  model_formula <- train2 %>% 
    # Take all predictor variable names and separate them with + signs:
    names() %>% 
    setdiff(c("id", "loss", "dummy_var")) %>% 
    stringr::str_c(collapse=" + ") %>% 
    # Add outcome variable and ~ sign and convert to formula
    stringr::str_c("dummy_var ~ ", .)
  model_formula
  model_formula <- as.formula(model_formula)

  
  # Used in glmnet() function for ridge regression and LASSO
  X <- model.matrix(model_formula, data = pseudo_train)[, -1]
  y <- pseudo_train$loss
  
  
  model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_star_ridge)
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star_lasso)
  model_lm <- lm(model_formula, data = pseudo_train)
  
  #create new data frame
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  
  
  # Get predictions for all three methods.
  lm_predictions <- model_lm %>% 
    predict(newdata = pseudo_test)
  
  ridge_predictions <- model_ridge %>%
    predict(newx=X_new, s= lambda_star_ridge)
  
  LASSO_predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star_lasso)
  
  # Compute MSE for each method and save them (by appending/binding to
  # results_all_folds)
  results_all_folds <- 
    # Create data frame of y=SalePrice and predictions from all three methods
    data_frame(
      lm = as.vector(lm_predictions),
      ridge = as.vector(ridge_predictions),
      LASSO = as.vector(LASSO_predictions)
    ) %>% 
    # Switch to tidy format so we can use group_by() to compute MSE's for all
    # three methods easily
    tidyr::gather(method, yhat) %>% 
    group_by(method) %>% 
    summarise(MAE = sum(abs(pseudo_test$loss-yhat)) / length(pseudo_test$loss)) %>% 
    # Include fold i variable and append/bind to results_all_folds
    mutate(fold=i) %>% 
    bind_rows(results_all_folds)
}

# Show results
results_all_folds %>% 
  group_by(method) %>% 
  summarise(MAE_CV = mean(MAE)) %>% 
  arrange(desc(MAE_CV))

#Since lambda_star is often very low, I was worried that I would be better served with a simpler model
#this confirms even a little shrinkage is fine
#even though i liked lasso more, ridge is better
#learn something new every day!


#---Can we make it better???---
#What I'm doing here - and you're going to have to take my word for it because im not making this a million lines long-
#is systematically removing variables to see what score we get without it
#i'm also switching from lasso to ridge because it scored better above ^^^
#1417.297 is the score to beat

# removing cat1  = 1437.506 
# removing cont2 = 1441.542
#note: i was getting weird error messages and they went away when i set folds = 5 at this round
#also note : since nothing is simple it doesn't always work to run this as one big chunk

# removing cont3 = 1428.585 ! (! means its lower than 1430, my arbitrary cutoff)
#removing cont7 =  1437.723
# removing cat7 = 1428.141 !
#removing cat10 = 1425.887 
# removing  cat12 = 1447.456
# removing cat23 =  1441.692
# removing cat28 = 1428.475 !
# removing cat36 =  1432.494
#removing cat40 = 1428.164 ! 
# removing cat41 = 1428.134 !
# removing cat50 = 1427.646 ! 
# removing cat57 =  1439.657
# removing cat70 = 1428.132 ! 
# removing cat72 = 1432.386
# removing cat80 = 1481.565 *****
# removing cat81 = 1452.331
# removing cat87 = 1434. 618
# removing cat94 = 1428.422 !
# removing cat100 = 1442.897

#SOME TAKEAWAYS
#good news is none of our variables are making our model worse. 80 is by far the most important
# but we want the SIMPLEST model possible while still giving us good results
#so sorry cont7, cat10, cat28, cat40, cat41, cat50, cat70, cat94
# when they're all out i get 1441.297 


train2<- train1 %>%
  select(id,loss, dummy_var, cat1, cont2, cont3, cat7, cat12, cat23, cat36,  
         cat57, cat72, cat80,  cat81, cat87, cat100)

#create model
model_formula <- train2 %>% 
  # Take all predictor variable names and separate them with + signs:
  names() %>% 
  setdiff(c("id", "loss", "dummy_var")) %>% 
  stringr::str_c(collapse=" + ") %>% 
  # Add outcome variable and ~ sign and convert to formula
  stringr::str_c("dummy_var ~ ", .)
model_formula
model_formula <- as.formula(model_formula)

X <- model.matrix(model_formula, data = train2)[, -1]
y <- train2$loss

lambda_values <- 10^seq(from = 2, to = 4, length = 100)
#BIG CHANGE to ridge regression
model_BIG <- glmnet(X, y, alpha = 0, lambda = lambda_values)


#I'm taking out the look at it part here as well
#we've looked at this a million times now
#i dont need more graphs


#cv
cvfit <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting

# if you get an error message try switching i for 1, running it, then switching it back
#idk why it works but it DOES
#also re-running this folds section works sometimes
n_folds<- 5
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(length(n_folds))


for(i in 1:n_folds){
  
  #create folds
  pseudo_train <- train2 %>%
    filter(fold != i)
  pseudo_test <- train2 %>%
    filter(fold == i)
  
  #pseudo_train model
  model_formula <- pseudo_train %>% 
    # Take all predictor variable names and separate them with + signs:
    names() %>% 
    setdiff(c("id", "loss", "dummy_var", "fold")) %>% 
    stringr::str_c(collapse=" + ") %>% 
    # Add outcome variable and ~ sign and convert to formula
    stringr::str_c("dummy_var ~ ", .)
  model_formula
  model_formula <- as.formula(model_formula)
  
  X <- model.matrix(model_formula, data = pseudo_train)[, -1]
  y <- pseudo_train$loss
  
  model_BIG <- glmnet(X, y, alpha = 0, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_BIG %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}

mean(results)
 # when i seriously shrink the variables i got 1441.297 - not as good as my optimal score but much simpler


# --- predictions---
test_X <- model.matrix(model_formula, data = test)[, -1]

predictions <- model_BIG %>%
  predict(newx= test_X, s = lambda_star)

# Write submissions to CSV
sample_sub %>% 
  mutate(loss = as.vector(predictions)) %>% 
  write_csv("~/Desktop/final_submission.csv")
#officially I got 1425.46 AKA not bad compared to 1417 and missing so many variables
#i'm still waaay behind the top guys (they have like 110) but i've improved so much so far!