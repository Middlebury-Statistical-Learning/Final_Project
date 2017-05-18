#SO I hate feeling overwhelmed and decided that my first final.r was long enough so now I have final part 2
#in this edition I try all the variables

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

#i knwo you run the risk of missing some but this helps it go much faster
train1 <- train %>%
  sample_frac(0.7)

#create dummy var (useful for lasso)
train1 <- train1 %>% 
  mutate(dummy_var = 1)
test <- test %>% 
  mutate(dummy_var = 1)


#I could try all the variables at once but that won't be efficient and i think some of the variables are faulty
#New strategy - systematically testing variables in bunches of 20, with CV
#goal is to run through all of the variables, see what combos work best, then try to make a super combination that isn't random
#wish  me luck

#--cat 1-20--
train2<- train1 %>%
  select(id,loss, dummy_var, num_range("cat", 1:20))
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
#i've always liked lasso more so im running with it
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#look @ it
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
#looks like cat1 and cat7 are especially important, then cat10 and cat12

#cv
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting
n_folds<- 4
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(4)

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
  
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}
mean(results)
#1631.285

#good news is nowt that I know this works I can just copy/paste the above code :) 

#--cat 21-40--
train2<- train1 %>%
  select(id,loss, dummy_var, num_range("cat", 21:40))
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
#i've always liked lasso more so im running with it
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#look @ it
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
#weird ok 
#cat23 and 36 and 40 and 28 most influential

#cv
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting
n_folds<- 4
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(4)

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
  
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}
mean(results)
#1863.217
#NOT AS GOOD AS FIRST 20. NOTED

#--cat 41-60--
train2<- train1 %>%
  select(id,loss, dummy_var, num_range("cat", 41:60))
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
#i've always liked lasso more so im running with it
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#look @ it
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
#WOW cat57 and cat50 are way ahead of everyone else. cat41 is a distant third

#cv
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting
n_folds<- 4
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(4)

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
  
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}
mean(results)
#1835.903
#better than second group but not better than the first



#--cat 61-80-- found problem in troubleshooting.r
#61-70 works
#71-72 works 73-74, 76-77
#75 is a problem, 
train2<- train1 %>%
  select(id,loss, dummy_var, num_range("cat", 61:72), cat74, cat73, num_range("cat", 76-80))

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
#i've always liked lasso more so im running with it
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#look @ it
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
#we got some wild ones
#cat 79 and 80 are the big ones, then 72

#cv
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting
n_folds<- 4
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(4)

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
  
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}
mean(results)

#1551.226 REALLY GOOD


#--cat81-100-- ALSO A PROBLEM HERE UGH!!!!
#88 is the problem, maybe also 89, 90, 92, 99
#93-94 worked, 95, 96, 97, 98, 100, 91 
train2<- train1 %>%
  select(id,loss, dummy_var, num_range("cat", 81:87), num_range("cat", 93:98), cat91, cat100)

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
#i've always liked lasso more so im running with it
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#look @ it
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
#87,81,100, maybe 94

#cv
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting
n_folds<- 4
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(4)

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
  
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}
mean(results)
#1663.659


#--cat100 - 116-- another E#E#E!!!
#101 could be a problem, same for 102-105
#this section has been the WORST only none of them worked
#i dont know why they don't work but im just going to pretend they don't exist :)
train2<- train1 %>%
  select(id,loss, dummy_var, num_range("cat", 101:116))


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
#i've always liked lasso more so im running with it
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#look @ it
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


#cv
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting
n_folds<- 4
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(4)

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
  
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}
mean(results)


#--cont1-14--
train2<- train1 %>%
  select(id,loss, dummy_var, contains("cont"))

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
#i've always liked lasso more so im running with it
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#look @ it
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
#it's only showing 2, 3, 7

#cv
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting
n_folds<- 4
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(4)

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
  
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}
mean(results)
#1947.52 aka the worst yet


#---LETS TRY SOME BIG COMBINATIONS---
#looks like cat1 and cat7 are especially important, then cat10 and cat12
#cat23 and 36 and 40 and 28 most influential
#WOW cat57 and cat50 are way ahead of everyone else. cat41 is a distant third
#cat 79 and 80 are the big ones, then 72
#87,81,100, maybe 94 
#do i include cont? this time I will just 2,3,7
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

lambda_values <- 10^seq(from = 2, to = 4, length = 100)
#i've always liked lasso more so im running with it
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

#look @ it
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
#80 is most, 81 least

#cv
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

# Optimal lambda from cross-validation!

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

#ok great now we get to cross validate to see how good our model is at predicting
#im adding more folds since this is BIG LEAGUE MODELING
n_folds<- 10
train2 <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)
results<- numeric(length(n_folds))


for(i in 1:n_folds){
  
  #create folds
  pseudo_train <- train2 %>%
    filter(fold != 1)
  pseudo_test <- train2 %>%
    filter(fold == 1)
  
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
  
  model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_star)
  
  #create predictions
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_lasso %>%
    predict(newx=X_new, s= lambda_star)
  pseudo_test <-pseudo_test %>%
    mutate(pred = as.vector(predictions))
  results[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
}
mean(results)

#1417.297 BEST YET

# --- predictions---

test_X <- model.matrix(model_formula, data = test)[, -1]

predictions <- model_ridge %>%
  predict(newx= test_X, s = lambda_star)

# Write submissions to CSV
sample_sub %>% 
  mutate(loss = as.vector(predictions)) %>% 
  write_csv("~/Desktop/final_submission.csv")
#i got a 1416 officially :) :) 