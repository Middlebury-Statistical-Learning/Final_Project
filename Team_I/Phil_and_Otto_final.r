#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Phil Hoxie and Otto Nagengast 
# Kaggle Competition Name: Prudential Life Insurance
# Kaggle Competition URL: https://www.kaggle.com/c/prudential-life-insurance-assessment/data
#-------------------------------------------------------------------------------

####################################################
# Please Look at our CART and KNN models in Extras #
# They took a very long time to make, even though  #
# they were not our final models. Thanks           #
####################################################

### Loading packages and getting set up ---------------------------------------------------------
setwd("~/Machine Learning/Final_Project/Team I")

library(tidyverse)
library(broom)
library(plotly)
library(glmnet)
library(stringr)
library(readr)
library(class)
library(Metrics)
library(rpart)


### Data ----------------------------------------------------------------------

train <- read.csv(file = "Data/train.csv", stringsAsFactors = FALSE)
test <- read.csv(file = "Data/test.csv", stringsAsFactors = FALSE)
sample_sub <- read.csv(file = "Data/sample_submission.csv", stringsAsFactors = FALSE)

# If you do not want to run the KNN and CART loops, 
# here are the results in .csv files below (commented out)
#
# Code should run after each write.csv() line for graphs
# Exact starting place specified in code
#
# scores_final_cart_tbl <- read.csv(file = "Data/scores_final_cart.csv", stringsAsFactors = FALSE)
# scores_final_knn_tbl <- read.csv(file = "Data/scores_final_knn.csv", stringsAsFactors = FALSE)

# Briefly peruse the data 
names(train)
summary(train$Response)

## Response is a discrete integer 

### Dealing with missing values ---------------------------------------------

## Na's by Var
## This creates a list that shows each variable and the number of missing values it has.  
na_count <-sapply(train, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

## This creates a list of variables with missing values. 
na_count_filt <- na_count %>% 
  filter(na_count!=0)

## Replacing missing values with the median of each 

# First, create a duplicate training dataset 
train1 = train

# Varlist contains all of the variables with missing values 
varlist <- c("Employment_Info_1", "Employment_Info_4", "Employment Info_6", 
             "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", 
             "Family_Hist_4", "Family_Hist_5", "Medical_History_1", 
             "Medical_History_10", "Medical_History_15", "Medical_History_24", 
             "Medical_History_32")

# One by one, manually replacing missing values with the median of each variable 
median_var <- median(train$Employment_Info_1, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Employment_Info_1 = ifelse(is.na(Employment_Info_1), median_var, Employment_Info_1)
  )

median_var <- median(train$Employment_Info_4, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Employment_Info_4 = ifelse(is.na(Employment_Info_4), median_var, Employment_Info_4)
  )

median_var <- median(train$Employment_Info_6, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Employment_Info_6 = ifelse(is.na(Employment_Info_6), median_var, Employment_Info_6)
  )

median_var <- median(train$Insurance_History_5, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Insurance_History_5 = ifelse(is.na(Insurance_History_5), median_var, Insurance_History_5)
  )

median_var <- median(train$Family_Hist_2, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Family_Hist_2 = ifelse(is.na(Family_Hist_2), median_var, Family_Hist_2)
  )

median_var <- median(train$Family_Hist_3, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Family_Hist_3 = ifelse(is.na(Family_Hist_3), median_var, Family_Hist_3)
  )

median_var <- median(train$Family_Hist_4, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Family_Hist_4 = ifelse(is.na(Family_Hist_4), median_var, Family_Hist_4)
  )

median_var <- median(train$Family_Hist_5, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Family_Hist_5 = ifelse(is.na(Family_Hist_5), median_var, Family_Hist_5)
  )

median_var <- median(train$Medical_History_1, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Medical_History_1 = ifelse(is.na(Medical_History_1), median_var, Medical_History_1)
  )

median_var <- median(train$Medical_History_10, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Medical_History_10 = ifelse(is.na(Medical_History_10), median_var, Medical_History_10)
  )

median_var <- median(train$Medical_History_15, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Medical_History_15 = ifelse(is.na(Medical_History_15), median_var, Medical_History_15)
  )

median_var <- median(train$Medical_History_24, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Medical_History_24 = ifelse(is.na(Medical_History_24), median_var, Medical_History_24)
  )

median_var <- median(train$Medical_History_32, na.rm=TRUE)
train1 <- train1 %>% 
  mutate(
    Medical_History_32 = ifelse(is.na(Medical_History_32), median_var, Medical_History_32)
  )

# Second, create a cleaned test dataset 
test1 <- test

# Varlist contains all of the variables with missing values 
varlist <- c("Employment_Info_1", "Employment_Info_4", "Employment Info_6", 
             "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", 
             "Family_Hist_4", "Family_Hist_5", "Medical_History_1", 
             "Medical_History_10", "Medical_History_15", "Medical_History_24", 
             "Medical_History_32")

# One by one, manually replacing missing values with the median of each variable 
median_var <- median(test$Employment_Info_1, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Employment_Info_1 = ifelse(is.na(Employment_Info_1), median_var, Employment_Info_1)
  )

median_var <- median(test$Employment_Info_4, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Employment_Info_4 = ifelse(is.na(Employment_Info_4), median_var, Employment_Info_4)
  )

median_var <- median(test$Employment_Info_6, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Employment_Info_6 = ifelse(is.na(Employment_Info_6), median_var, Employment_Info_6)
  )

median_var <- median(test$Insurance_History_5, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Insurance_History_5 = ifelse(is.na(Insurance_History_5), median_var, Insurance_History_5)
  )

median_var <- median(test$Family_Hist_2, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Family_Hist_2 = ifelse(is.na(Family_Hist_2), median_var, Family_Hist_2)
  )

median_var <- median(test$Family_Hist_3, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Family_Hist_3 = ifelse(is.na(Family_Hist_3), median_var, Family_Hist_3)
  )

median_var <- median(test$Family_Hist_4, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Family_Hist_4 = ifelse(is.na(Family_Hist_4), median_var, Family_Hist_4)
  )

median_var <- median(test$Family_Hist_5, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Family_Hist_5 = ifelse(is.na(Family_Hist_5), median_var, Family_Hist_5)
  )

median_var <- median(test$Medical_History_1, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Medical_History_1 = ifelse(is.na(Medical_History_1), median_var, Medical_History_1)
  )

median_var <- median(test$Medical_History_10, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Medical_History_10 = ifelse(is.na(Medical_History_10), median_var, Medical_History_10)
  )

median_var <- median(test$Medical_History_15, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Medical_History_15 = ifelse(is.na(Medical_History_15), median_var, Medical_History_15)
  )

median_var <- median(test$Medical_History_24, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Medical_History_24 = ifelse(is.na(Medical_History_24), median_var, Medical_History_24)
  )

median_var <- median(test$Medical_History_32, na.rm=TRUE)
test1 <- test1 %>% 
  mutate(
    Medical_History_32 = ifelse(is.na(Medical_History_32), median_var, Medical_History_32)
  )

## Looking to see if there's any weird stuff going on with any of the variables. 

# Varlist contains all of the variables with missing values 
varlist_NAs <- c("Employment_Info_1", "Employment_Info_4", "Employment_Info_6", 
                 "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", 
                 "Family_Hist_4", "Family_Hist_5", "Medical_History_1", 
                 "Medical_History_10", "Medical_History_15", "Medical_History_24", 
                 "Medical_History_32")

varlist_all <- c("Id", "Product_Info_1", "Product_Info_2", "Product_Info_3", 
                 "Product_Info_4", "Product_Info_5", "Product_Info_6", "Product_Info_7", 
                 "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_2", 
                 "Employment_Info_3", "Employment_Info_4", "Employment_Info_5", 
                 "Employment_Info_6", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", 
                 "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", 
                 "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", 
                 "Insurance_History_4", "Insurance_History_5", "Insurance_History_7", 
                 "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", 
                 "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", "Family_Hist_5", 
                 "Medical_History_1", "Medical_History_2", "Medical_History_3", 
                 "Medical_History_4", "Medical_History_5", "Medical_History_6", 
                 "Medical_History_7", "Medical_History_8", "Medical_History_9", 
                 "Medical_History_10", "Medical_History_11", "Medical_History_12", 
                 "Medical_History_13", "Medical_History_14", "Medical_History_15", 
                 "Medical_History_16", "Medical_History_17", "Medical_History_18", 
                 "Medical_History_19", "Medical_History_20", "Medical_History_21", 
                 "Medical_History_22", "Medical_History_23", "Medical_History_24", 
                 "Medical_History_25", "Medical_History_26", "Medical_History_27", 
                 "Medical_History_28", "Medical_History_29", "Medical_History_30", 
                 "Medical_History_31", "Medical_History_32", "Medical_History_33", 
                 "Medical_History_34", "Medical_History_35", "Medical_History_36", 
                 "Medical_History_37", "Medical_History_38", "Medical_History_39", 
                 "Medical_History_40", "Medical_History_41", "Medical_Keyword_1", 
                 "Medical_Keyword_2", "Medical_Keyword_3", "Medical_Keyword_4", 
                 "Medical_Keyword_5", "Medical_Keyword_6", "Medical_Keyword_7", 
                 "Medical_Keyword_8", "Medical_Keyword_9", "Medical_Keyword_10", 
                 "Medical_Keyword_11", "Medical_Keyword_12", "Medical_Keyword_13", 
                 "Medical_Keyword_14", "Medical_Keyword_15", "Medical_Keyword_16", 
                 "Medical_Keyword_17", "Medical_Keyword_18", "Medical_Keyword_19", 
                 "Medical_Keyword_20", "Medical_Keyword_21", "Medical_Keyword_22", 
                 "Medical_Keyword_23", "Medical_Keyword_24", "Medical_Keyword_25", 
                 "Medical_Keyword_26", "Medical_Keyword_27", "Medical_Keyword_28", 
                 "Medical_Keyword_29", "Medical_Keyword_30", "Medical_Keyword_31", 
                 "Medical_Keyword_32", "Medical_Keyword_33", "Medical_Keyword_34", 
                 "Medical_Keyword_35", "Medical_Keyword_36", "Medical_Keyword_37", 
                 "Medical_Keyword_38", "Medical_Keyword_39", "Medical_Keyword_40", 
                 "Medical_Keyword_41", "Medical_Keyword_42", "Medical_Keyword_43", 
                 "Medical_Keyword_44", "Medical_Keyword_45", "Medical_Keyword_46", 
                 "Medical_Keyword_47", "Medical_Keyword_48", "Response")

for (i in varlist_all) {
  print(i)
  summary(train1[,i]) %>% print()
}

# this is cleaner. I love loops 

vars <- names(train)
sum_mat <- NULL
for(i in 2:128) {
  temp_v <- rep(0,4)
  temp_v[1] <- min(train_eda[,i])
  temp_v[2] <- median(train_eda[,i])
  temp_v[3] <- mean(train_eda[,i])
  temp_v[4] <- max(train_eda[,i])
  temp_v <- temp_v %>% 
    tbl_df()
  sum_mat <- bind_cols(sum_mat, temp_v)
}
dim(sum_mat)
colnames(sum_mat) <- vars[2:128]
rownames(sum_mat) <- c("min", "median", "mean", "max")

sum_mat <- t(sum_mat) %>% 
  tidy() %>% 
  tbl_df()

# Now we can easily look at each variable
head(sum_mat)

### Exploratory data analysis ----------------------------------------------------------

## Use LASSO to identify key predictors
train_eda <- train1

# Phil, what's this doing? # 
formula_all <-as.formula(paste(vars[128], 
                               paste(vars[2:127], collapse = " + "), sep = " ~ "))

X <- model.matrix(formula_all, data = train_eda)[, -1]
y <- train_eda$Response

mean(y)

lambda_values <- 10^seq(10, -2, length = 100)
lambda_values
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_values)

## Making a plot to show shrinkage
model_LASSO %>%
  tidy() %>%
  tbl_df() %>%
  # lambda's on x-axis are better viewed on a log-scale:
  mutate(log_lambda = log(lambda)) %>%
  # We're not interested in the intercept estimate beta_0_hat
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=log_lambda, y=estimate, col=term)) +
  geom_line()

# Still a bit hard to read because of multiple colors. Let's make this interactive:
ggplotly()

## The 10 variables that shrink the slowest: 
# BMI, Medical_History_23, Medical_Keyword_3, Medical_History_4, Product_Info_4, 
# Wt, Ins_Age, Medical_History_40, Product_info_2A7, and Medical_History_13

## Let's make a quick scatter to see what we have 

## Scatter plot of BMI and Age 
BMI_Age_Scatter <- ggplot(data = train_eda, 
                          aes(x = BMI, y = Ins_Age, group = factor(Response)))+
  geom_point(aes(color = factor(Response)))
BMI_Age_Scatter

# Some clustering, a lot of overlap. 

### Principal Component Analysis (PCA)  ----------------------------------
# We use PCA to reduce the number of dimensions to reduce computational time. 
# you may need to take out the '-Product_info_2A7' because I added it
train_pca <- train1 %>% 
  select(-Id)

PI_2 <- train_pca %>% 
  group_by(Product_Info_2) %>% 
  tally()
PI_2

PI_2levels <- PI_2$Product_Info_2
PI_2levels

train_pca <- train_pca %>% 
  mutate(A1 = ifelse(Product_Info_2=="A1", 1, 0),
         A2 = ifelse(Product_Info_2=="A2", 1, 0),
         A3 = ifelse(Product_Info_2=="A3", 1, 0),
         A4 = ifelse(Product_Info_2=="A4", 1, 0),
         A5 = ifelse(Product_Info_2=="A5", 1, 0),
         A6 = ifelse(Product_Info_2=="A6", 1, 0),
         A7 = ifelse(Product_Info_2=="A7", 1, 0),
         A8 = ifelse(Product_Info_2=="A8", 1, 0),
         B1 = ifelse(Product_Info_2=="B1", 1, 0),
         B2 = ifelse(Product_Info_2=="B2", 1, 0),
         C1 = ifelse(Product_Info_2=="C1", 1, 0),
         C2 = ifelse(Product_Info_2=="C2", 1, 0),
         C3 = ifelse(Product_Info_2=="C3", 1, 0),
         C4 = ifelse(Product_Info_2=="C4", 1, 0),
         D1 = ifelse(Product_Info_2=="D1", 1, 0),
         D2 = ifelse(Product_Info_2=="D2", 1, 0),
         D3 = ifelse(Product_Info_2=="D3", 1, 0),
         D4 = ifelse(Product_Info_2=="D4", 1, 0),
         E1 = ifelse(Product_Info_2=="E1", 1, 0)
  )
dim(train_pca)
response <- train_pca$Response %>% 
  tidy() %>% 
  tbl_df()
train_pca <- train_pca %>% 
  select(-Product_Info_2, -Response)

de_mean <- function(x){
  output <- x - mean(x)
  return(output)
}

train_pca <- train_pca %>% 
  mutate_each(funs(de_mean))

cov(train_pca) %>% round(2)

eigen <- cov(train_pca) %>% eigen()
eigen_vals <- eigen$values
Gamma <- eigen$vectors

Y <- as.matrix(train_pca) %*% Gamma %>% 
  as_data_frame()

glimpse(Y)

var_Y <- cov(Y) %>% diag()
cumsum(var_Y)/sum(var_Y)

dim(Y)
dim(response)

PC_Data <- bind_cols(response, Y)

PC_Data <- PC_Data %>% 
  rename(Response = x)

pc_data <- PC_Data %>% 
  select(Response, V1, V2, V3)

PC_Plot <- ggplot(data = PC_Data, 
                  aes(x = V1, 
                      y = V2,
                      group = factor(Response)))+
  geom_point(aes(color = factor(Response)))
PC_Plot

# Not much better than before. No obvious clusters seem to form

### Models ---------------------------------------------------

# Creating a vector that contains the best specification of each model 
best_of_each <- rep(0, 3) 

### LASSO --------------------------------------------------------------------------------
train_lasso <- train1

vars <- names(train)

# Model formula 
formula_all <-as.formula(paste(vars[128], 
                               paste(vars[2:127], collapse = " + "), sep = " ~ "))

# Required for LASSO model code 
X <- model.matrix(formula_all, data = train_lasso)[, -1]
y <- train_lasso$Response
Resp <- train_lasso$Response %>% 
  tidy() %>% 
  tbl_df()

# Vector of lambda values to try 
lambda_values <- 10^seq(1, -4, length = 100)

# Fitting the model to train 
model_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_values)

# Find the optimal lambda value 
cvfit <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

cvfit %>%
  glance()
lambda_star <- cvfit %>%
  glance() %>%
  .[["lambda.min"]]
lambda_star

cv_results <- cvfit %>%
  tidy() %>%
  tbl_df() %>%
  mutate(log_lambda=log(lambda))

ggplotly(ggplot(cv_results, aes(x=log_lambda)) +
           geom_point(aes(y=estimate)) +
           labs(x="log(lambda)", y="Cross-Validated MSE") +
           # Optional: Errorbars on cross-validated MSE's
           # geom_errorbar(aes(ymin=conf.low, ymax=conf.high)) +
           geom_vline(xintercept = log(lambda_star), col="red")
)


## Making predictions 

# Create predictor matrix as before
X_new <- model.matrix(formula_all, data = train_lasso)[, -1]

# Get predictions
preds <- predict(model_lasso, newx=X, s=lambda_star, type="class") 

train_lasso <- train_lasso %>% 
  mutate(Predictions = as.numeric(preds)) %>% 
  mutate(Predictions = ifelse(Predictions<1, 1, Predictions)) %>% 
  mutate(Predictions = ifelse(Predictions>8, 8, Predictions))

train_lasso %>% 
  select(Predictions, Response) 


# Convert predictions from continuous to factor variable 
train_lasso <- train_lasso %>% 
  mutate(
    Predictions = round(Predictions)
  )

# Get score 
lasso_best_score <- ScoreQuadraticWeightedKappa(train_lasso$Predictions, as.numeric(train_lasso$Response))

# Storing the score of the best LASSO specification 
best_of_each[1] <- lasso_best_score

lasso_best_score

# The best score is the score generated by LASSO with 
# lambda = lambda_star(0.549). 

### Getting predictions on test with LASSO ------------------------------------------------------------
vars <- names(test)

# Model formula (from before)
formula_all

# Required for LASSO model code 
test1 <- test1 %>% 
  mutate(Response = 1)

# Making predictions 
X_new <- model.matrix(formula_all, data = test1)[, -1]
preds <- predict(model_lasso, newx=X_new, s=lambda_star, type="class") 
test1 <- test1 %>% 
  mutate(Predictions = as.numeric(preds)) %>% 
  mutate(Predictions = ifelse(Predictions<1, 1, Predictions)) %>% 
  mutate(Predictions = ifelse(Predictions>8, 8, Predictions))

head(test1$Predictions)

#test1 %>% 
#  select(Predictions, Response) %>% 
#  View()

# Convert predictions from continuous to factor variable 
test1 <- test1 %>% 
  mutate(
    Predictions = round(Predictions)
  )

head(test1$Predictions)

# Creating a subsmission file
submission <- test1 %>% 
  dplyr::rename(Response_actual = Response) %>% 
  dplyr::rename(Response = Predictions) %>% 
  select(Id, Response)

submission %>% 
  mutate(Response = as.integer(Response)) %>% 
  write_csv("submission_check.csv") 

## Kaggle scores (QWKappa)
# Private: 0.56101
# Public: 0.54504

min(submission$Response)
max(submission$Response)

### Extras-------------------------------------------------

# Other Models, KNN and CART

### K-Nearest Neighbors -------------------------------------------------------

# We set the random number generator seed value to get replicable randomness
set.seed(76)

# Creating the training dataset for CART 
train_knn <- pc_data

# Define number of neighbors:
num_neigh <- seq(1, 100, by = 1)

# Storing the score for each k here 
scores_final_knn <- rep(0, length(num_neigh))

# in 1:num_neigh
for (j in 1:length(num_neigh))  { 
  
  # Create pseudo_train and pseudo_test set 
  # k=5 Fold CV
  # Assign folds at random
  n_folds <- 5
  train_knn <- train_knn %>%
    mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(train)))
  
  # Storing the scores for each fold for each method here: 
  scores_fold <- rep(0, n_folds)
  
  for(i in 1:n_folds) {
    # Create disjoint pseudo-train and pseudo-test sets based on folding scheme
    pseudo_train <- train_knn %>%
      filter(fold != i)
    pseudo_test <- train_knn %>%
      filter(fold == i)
    
    ## Fit model & get predictions
    
    # Assign categorical outcome variable:
    classifications <- pseudo_train$Response
    
    # Note the training data and test data inputs to knn() have to be the same size
    # and just be the predictor variables
    train_input <- pseudo_train %>%
      select(V1, V2, V3)
    test_input <- pseudo_test %>%
      select(V1, V2, V3)
    
    # Fit model to pseudo_train and generate predictions on pseudo_test 
    model_knn <-
      class::knn(train=train_input, test=test_input, cl=classifications, k = num_neigh[j])
    
    # Convert "model_knn" from factor to numeric 
    model_knn <- as.numeric(model_knn)
    
    # Add predictions and probabilities to pseudo_test set
    pseudo_test <- pseudo_test %>%
      mutate(Predictions = model_knn)
    
    # Get score 
    ScoreQuadraticWeightedKappa(pseudo_test$Predictions,as.numeric(pseudo_test$Response))
    
    # Store the score of each fold  
    scores_fold[i] <- ScoreQuadraticWeightedKappa(pseudo_test$Predictions,as.numeric(pseudo_test$Response))
  }
  # Take the mean of these scores to find the score for a given k-neighbors
  scores_final[j] <- mean(scores_fold)
  print(j)
}

scores_final_knn <- scores_final

# Saving these results  
scores_final_knn_tbl <- scores_final_knn %>% 
  tidy() %>% 
  tbl_df()

scores_final_knn_tbl %>% 
  write_csv("scores_final_knn.csv")  

## If you did not run the loop, start here
## Visualizing Optimal K (Maximizing QWKappa Score)

num_neigh <- seq(1, 100, by = 1)

scores_final_KNN_QWK <- scores_final_knn_tbl %>% 
  rename(QWKappa = x)

num_neigh <- num_neigh %>% 
  tidy() %>% 
  tbl_df() 
num_neigh <- num_neigh %>% 
  rename(k = x)

KNN_CV_Scores <- bind_cols(num_neigh, scores_final_KNN_QWK)

max(KNN_CV_Scores$QWKappa)

K_star <- KNN_CV_Scores %>% 
  filter(QWKappa == max(KNN_CV_Scores$QWKappa))

ggplot(data = KNN_CV_Scores, aes(x=k, y=QWKappa))+
  geom_point()+
  geom_vline(xintercept = K_star$k, col = "blue")

# Picking out the best KNN specification 
scores_final_knn <- read.csv(file = "scores_final_knn.csv", stringsAsFactors = FALSE)
best_of_each[2] <- max(scores_final_knn)
max(scores_final_knn)


### CART ---------------------------------------------------------------------

# Creating the training dataset for CART 
train_cart <- pc_data

# Creating the model formula 
model_formula <- as.formula("Response ~ V1 + V2 + V3")

# Setting the max depth. This will produce a big tree, but we're going to prune it. 
depth <- 200

# Complexity parameter values
CP <- 10^seq(-0.5, -3, length = 50)

# Storing the score for each value of the CP
scores_final_cart <- rep(0, length(CP))

# Optimizing the model through CV 
for (j in 1:length(CP)) {
  
  # Create pseudo_train and pseudo_test set 
  # k=5 Fold CV
  # Assign folds at random
  n_folds <- 5
  train_cart <- train_cart %>%
    mutate(fold = sample(1:n_folds, replace=TRUE, size=nrow(train_cart)))
  
  # Storing the scores for each fold for each method here: 
  scores_fold <- rep(0, n_folds)
  
  for(i in 1:n_folds) {
    # Create disjoint pseudo-train and pseudo-test sets based on folding scheme
    pseudo_train <- train_cart %>%
      filter(fold != i)
    pseudo_test <- train_cart %>%
      filter(fold == i)
    
    # Fit the model to pseudo_train
    model_cart <- rpart(model_formula, data = pseudo_train, method="class", control=depth)
    
    # Pruning the tree 
    model_cart_pruned <- prune(model_cart, cp=CP[j])
    
    # Generating predictions 
    pseudo_test <- pseudo_test %>% 
      mutate(Predictions = predict(model_cart_pruned, newdata=pseudo_test, type="vector"))     
    
    # Get the score 
    ScoreQuadraticWeightedKappa(pseudo_test$Predictions, as.numeric(pseudo_test$Response))
    
    # Store the score of each fold  
    scores_fold[i] <- ScoreQuadraticWeightedKappa(pseudo_test$Predictions,as.numeric(pseudo_test$Response))
  }
  
  # Take the mean of the scores from each to find the score for each cp
  scores_final_cart[j] <- mean(scores_fold)
  print(j)
}

# Saving these results  
scores_final_cart_tbl <- scores_final_cart %>% 
  tidy() %>% 
  tbl_df()
scores_final_cart_tbl %>%  
  write_csv("~/scores_final_cart.csv")  

## If you did not run loop, start here
## Visualize optimal CP

CP <- 10^seq(-0.5, -3, length = 50)
scores_final_cart_QWK <- scores_final_cart_tbl %>% 
  rename(QWKappa = x)

CP_tbl <- CP %>% 
  tidy() %>% 
  tbl_df() 
CP_tbl <- CP_tbl %>% 
  rename(CP = x)

CART_CV_Scores <- bind_cols(CP_tbl, scores_final_cart_QWK)

max(CART_CV_Scores$QWKappa)

CP_star <- CART_CV_Scores %>% 
  filter(QWKappa == max(CART_CV_Scores$QWKappa))
ggplot(data = CART_CV_Scores, aes(x=CP, y=QWKappa))+
  geom_point()+
  geom_vline(xintercept = CP_star$CP, col = "blue")


# Picking out the best KNN specification
scores_final_knn <- read.csv(file = "scores_final_cart.csv", stringsAsFactors = FALSE)
best_of_each[3] <- max(scores_final_knn)
max(scores_final_knn)  

### Picking out the very best model ------------------------------------------------------
best_of_each 

# Some plots (We made a lot of plots like these)

BMI_Resp_plot <- ggplot(data = train_eda, 
                        aes(x = factor(Response), 
                            y = BMI))+
  geom_boxplot()+
  facet_grid(.~factor(Response), scales="free")+
  labs(x="Response")
BMI_Resp_plot


Wt_Resp_plot <- ggplot(data = train_eda, 
                       aes(x = factor(Response), 
                           y = Wt))+
  geom_boxplot()+
  facet_grid(.~factor(Response), scales="free")+
  labs(x="Response")
Wt_Resp_plot
