#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Ryan Rizzo
# Kaggle Competition Name: Allstate Severity Claims
# Kaggle Competition URL: https://www.kaggle.com/c/allstate-claims-severity/leaderboard
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------

library(tidyverse)
library(stringr)
library(broom)
library(glmnet)
library(plotly)

# 2. Load Data Files & Data Cleaning --------------------------------------
train <- read_csv("Files/train.csv")
test <- read_csv("Files/test.csv")
submission <- read_csv("Files/sample_submission.csv")

train2 <- train 

# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------

boxplot(loss~ cat1, data=train, 
        xlab="cat1", ylab="loss")

boxplot(loss~ cat7, data=train, 
        xlab="cat7", ylab="loss")

ggplot(train, aes(x=cont2, y=loss)) +
  geom_point()

ggplot(train, aes(x=cont13, y=loss)) +
  geom_point()


#splits training set upo into continuous and categorical variables seperately

#selected variables based on continuous vs categorical (included all continuous variables) and for categorical, I included most which I found had greater importance, but had to leave out some that had missing values.
train2 <- train1  %>%
  select(id,loss, cat1, cont2, cont3, cont4, cont5, cont6, cont7, cont8, cont10, cont11, cont12, cont13, cont14, cat7, cat10, cat11, cat12, cat23, cat36,  
         cat57, cat72, cat80,  cat81, cat82, cat87, cat100)

# 4. Cross-Validation of Final Model --------------------------------------

model_formula <- train2 %>%
  names() %>% 
  setdiff(c("id", "loss")) %>% 
  stringr::str_c(collapse=" + ") %>%
  stringr::str_c("loss ~ ", .)
model_formula <- as.formula(model_formula)

X <- model.matrix(model_formula, data = train2)[, -1]
Y <- train2$loss



#set lambdas at reasonable numbers
lambda_vals <- 10^seq(from = 1.5, to = 4, length = 50)

cvfit <- cv.glmnet(X, Y, alpha = 0, lambda= lambda_vals, nfolds = 5)
cvfit

#find the optimal
cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

model_LASSO <- glmnet(X, Y, alpha = 1, lambda = lambda_vals)

#plot coefficients
model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  #log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # remove intercept
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()



# Add folds and randomize
n_folds <- 5
trainset <- train2 %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n()))

# Save scores here
lasso_score_vector <- rep(0, n_folds)
ridge_score_vector <- rep(0, n_folds)

for(i in 1:n_folds){
  # 1. Create pseudo train/test based on folding scheme
  pseudo_train <- trainset %>%
    filter(fold != i)
  pseudo_test <- trainset %>%
    filter(fold == i)
  
  # 2. Fit model on training data
  loopX <- model.matrix(model_formula, data = pseudo_train)[, -1]
  loopY <- pseudo_train$loss
  
  #compare the ridge and the LASSO
  model_ridge <- glmnet(loopX, loopY, alpha = 0, lambda = lambda_star)
  
  new_LASSO <- glmnet(loopX, loopY, alpha = 1, lambda = lambda_star)
  

  # 3. Make predictions on test data & evaluate score
  ridgeX <- model.matrix(model_formula, data = pseudo_test)[, -1]
  predictions <- model_ridge %>%
    predict(newx= ridgeX, s= lambda_star)
  pseudo_test <- pseudo_test %>%
    mutate(pred = as.vector(predictions))
  ridge_score_vector[i]<- sum(abs(pseudo_test$loss-pseudo_test$pred)) / length(pseudo_test$loss)
  

  lassoX <- model.matrix(model_formula, data = pseudo_test)[, -1]
  LASSO_predictions <- new_LASSO %>%
    predict.glmnet(newx= lassoX, s = lambda_star)
  pseudo_test <- pseudo_test %>%
    mutate(lpred = as.vector(LASSO_predictions))
  
  lasso_score_vector[i]<- sum(abs(pseudo_test$loss-pseudo_test$lpred)) / length(pseudo_test$loss)
}

# Estimated score- LASSO came out on top
mean(ridge_score_vector)

mean(lasso_score_vector)


#1422.2
#On kaggle, got a 1405, which is close



# 5. Create Submission ----------------------------------------------------

test$loss = 0
  
test_X <- model.matrix(model_formula, data = test)[, -1]

LASSO_predictions <- predict(model_LASSO, s = lambda_star, newx = test_X, type = "response")


submission$loss = as.vector(LASSO_predictions)

readr::write_csv(submission, path="Files/allstate_submission.csv")








# 6. Extras ---------------------------------------------------------------

# Section A



# Section B



# Section C

