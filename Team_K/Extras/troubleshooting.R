#this is my troubleshooting section
#i have a feeling that some variables are faulty. Here I'm going to find out which

library(tidyverse)
library(stringr)
library(broom)
library(glmnet)
library(plotly)

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

#the problems are found 61-80 (done)
#also 81-100 (done)
#and somewhere betweeen 101 and 116

train2<- train1 %>%
  select(id,loss, dummy_var,cat100, cat106)

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

#skippign the looking at it phase bc i'm just looking for the error
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

