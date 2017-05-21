#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Alexander Pastora
# Kaggle Competition Name: Forest Cover Type Prediction
# Kaggle Competition URL: https://www.kaggle.com/c/forest-cover-type-prediction
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(broom)
library(ggplot2)
library(forcats)
library(reshape2)
library(glmnet)
library(class)





# 2. Load Data Files & Data Cleaning --------------------------------------
train <- read_csv("Files/train.csv")
test <- read_csv("Files/test.csv")

train <- train %>% mutate(Distance_To_Hydrology= sqrt((
  Horizontal_Distance_To_Hydrology)^2 + (Vertical_Distance_To_Hydrology)^2 ))

train <- train %>% mutate(Avg_Hillshade=(Hillshade_9am+Hillshade_Noon+Hillshade_3pm)/3)

wilderness <- train %>% select(Wilderness_Area1,Wilderness_Area2,Wilderness_Area3,
                 Wilderness_Area4)

soil <- train %>% select(Soil_Type1,Soil_Type2,Soil_Type3,Soil_Type4,Soil_Type5,
                         Soil_Type6,Soil_Type7,Soil_Type8,Soil_Type9,Soil_Type10,
                         Soil_Type11,Soil_Type12,Soil_Type13,Soil_Type14,Soil_Type15,
                         Soil_Type16,Soil_Type17,Soil_Type18,Soil_Type19,Soil_Type20,
                         Soil_Type21,Soil_Type22,Soil_Type23,Soil_Type24,Soil_Type25,
                         Soil_Type26,Soil_Type27,Soil_Type28,Soil_Type29,Soil_Type30,
                         Soil_Type31,Soil_Type32,Soil_Type33,Soil_Type34,Soil_Type35,
                         Soil_Type36,Soil_Type37,Soil_Type38,Soil_Type39,Soil_Type40)

train <- train %>% mutate(Wilderness_Area=names(wilderness)[row(t(wilderness))[t(wilderness)==1]])
train <- train %>% mutate(Soil_Type=names(soil)[row(t(soil))[t(soil)==1]])

train <- train %>% mutate(Wilderness_Area=as.numeric(fct_recode(Wilderness_Area,"1"="Wilderness_Area1",
                              "2"="Wilderness_Area2",
                              "3"= "Wilderness_Area3",
                              "4"="Wilderness_Area4")))

train <- train %>% mutate(Soil_Type=as.numeric(fct_recode(Soil_Type,
                                               "1"="Soil_Type1",
                                               "2"="Soil_Type2",
                                               "3"="Soil_Type3",
                                               "4"="Soil_Type4",
                                               "5"="Soil_Type5",
                                               "6"="Soil_Type6",
                                               "7"="Soil_Type7",
                                               "8"="Soil_Type8",
                                               "9"="Soil_Type9",
                                               "10"="Soil_Type10",
                                               "11"="Soil_Type11",
                                               "12"="Soil_Type12",
                                               "13"="Soil_Type13",
                                               "14"="Soil_Type14",
                                               "15"="Soil_Type15",
                                               "16"="Soil_Type16",
                                               "17"="Soil_Type17",
                                               "18"="Soil_Type18",
                                               "19"="Soil_Type19",
                                               "20"="Soil_Type20",
                                               "21"="Soil_Type21",
                                               "22"="Soil_Type22",
                                               "23"="Soil_Type23",
                                               "24"="Soil_Type24",
                                               "25"="Soil_Type25",
                                               "26"="Soil_Type26",
                                               "27"="Soil_Type27",
                                               "28"="Soil_Type28",
                                               "29"="Soil_Type29",
                                               "30"="Soil_Type30",
                                               "31"="Soil_Type31",
                                               "32"="Soil_Type32",
                                               "33"="Soil_Type33",
                                               "34"="Soil_Type34",
                                               "35"="Soil_Type35",
                                               "36"="Soil_Type36",
                                               "37"="Soil_Type37",
                                               "38"="Soil_Type38",
                                               "39"="Soil_Type39",
                                               "40"="Soil_Type40")))

test <- test %>% mutate(Distance_To_Hydrology= sqrt((
                                                 Horizontal_Distance_To_Hydrology)^2 + (Vertical_Distance_To_Hydrology)^2 ))

test <- test %>% mutate(Avg_Hillshade=(Hillshade_9am+Hillshade_Noon+Hillshade_3pm)/3)

wilderness_test <- test %>% select(Wilderness_Area1,Wilderness_Area2,Wilderness_Area3,
                               Wilderness_Area4)

soil_test <- test %>% select(Soil_Type1,Soil_Type2,Soil_Type3,Soil_Type4,Soil_Type5,
                         Soil_Type6,Soil_Type7,Soil_Type8,Soil_Type9,Soil_Type10,
                         Soil_Type11,Soil_Type12,Soil_Type13,Soil_Type14,Soil_Type15,
                         Soil_Type16,Soil_Type17,Soil_Type18,Soil_Type19,Soil_Type20,
                         Soil_Type21,Soil_Type22,Soil_Type23,Soil_Type24,Soil_Type25,
                         Soil_Type26,Soil_Type27,Soil_Type28,Soil_Type29,Soil_Type30,
                         Soil_Type31,Soil_Type32,Soil_Type33,Soil_Type34,Soil_Type35,
                         Soil_Type36,Soil_Type37,Soil_Type38,Soil_Type39,Soil_Type40)

test <- test %>% mutate(Wilderness_Area=names(wilderness_test)[row(t(wilderness_test))[t(wilderness_test)==1]])
test <- test %>% mutate(Soil_Type=names(soil_test)[row(t(soil_test))[t(soil_test)==1]])

test <- test %>% mutate(Wilderness_Area=as.numeric(fct_recode(Wilderness_Area,"1"="Wilderness_Area1",
                                                                "2"="Wilderness_Area2",
                                                                "3"= "Wilderness_Area3",
                                                                "4"="Wilderness_Area4")))

test <- test %>% mutate(Soil_Type=as.numeric(fct_recode(Soil_Type,
                                                          "1"="Soil_Type1",
                                                          "2"="Soil_Type2",
                                                          "3"="Soil_Type3",
                                                          "4"="Soil_Type4",
                                                          "5"="Soil_Type5",
                                                          "6"="Soil_Type6",
                                                          "7"="Soil_Type7",
                                                          "8"="Soil_Type8",
                                                          "9"="Soil_Type9",
                                                          "10"="Soil_Type10",
                                                          "11"="Soil_Type11",
                                                          "12"="Soil_Type12",
                                                          "13"="Soil_Type13",
                                                          "14"="Soil_Type14",
                                                          "15"="Soil_Type15",
                                                          "16"="Soil_Type16",
                                                          "17"="Soil_Type17",
                                                          "18"="Soil_Type18",
                                                          "19"="Soil_Type19",
                                                          "20"="Soil_Type20",
                                                          "21"="Soil_Type21",
                                                          "22"="Soil_Type22",
                                                          "23"="Soil_Type23",
                                                          "24"="Soil_Type24",
                                                          "25"="Soil_Type25",
                                                          "26"="Soil_Type26",
                                                          "27"="Soil_Type27",
                                                          "28"="Soil_Type28",
                                                          "29"="Soil_Type29",
                                                          "30"="Soil_Type30",
                                                          "31"="Soil_Type31",
                                                          "32"="Soil_Type32",
                                                          "33"="Soil_Type33",
                                                          "34"="Soil_Type34",
                                                          "35"="Soil_Type35",
                                                          "36"="Soil_Type36",
                                                          "37"="Soil_Type37",
                                                          "38"="Soil_Type38",
                                                          "39"="Soil_Type39",
                                                          "40"="Soil_Type40")))








# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------

# Hell Yes
ggplot(train,aes(x=Cover_Type,y=Elevation, group=Cover_Type))+ 
  geom_boxplot()

# Decent
ggplot(train,aes(x=Cover_Type,y=train$Horizontal_Distance_To_Roadways, group=Cover_Type))+ 
  geom_boxplot()

# Decent
ggplot(train,aes(x=Cover_Type,y=Distance_To_Hydrology, group=Cover_Type))+ 
  geom_boxplot()

# cool
ggplot(train,aes(x=Cover_Type,y=Wilderness_Area, group=Cover_Type))+ 
  geom_count()

#Use It
ggplot(train,aes(x=Cover_Type,y=Soil_Type, group=Cover_Type))+ 
  geom_count()










# 4. Cross-Validation of Final Model --------------------------------------
# NOTE: The Cross-Validation takes forever to run, so the code provided below
#       only illustrates what I would do in order to find the k-star value
#       for this model. In actuality, the k-star value used will just be
#       arbitrarily chosen. 
# K-Nearest Neighbors
k_values <- as.vector(1:15120)
n_folds <-10
train <- train %>% mutate(fold = rep(1:n_folds, length=n()))
classifications <- train$Cover_Type

for(i in 1:15120)
{
  scores <- as.vector(1:n_folds)
  for(j in 1:n_folds)
  {
    
    
    pseudo_train <- train %>%
      filter(fold != j) %>% select(Elevation,Horizontal_Distance_To_Roadways,
                                   Distance_To_Hydrology,Wilderness_Area,
                                   Soil_Type)
    pseudo_test <- train %>%
      filter(fold == j) %>% select(Elevation,Horizontal_Distance_To_Roadways,
                                   Distance_To_Hydrology,Wilderness_Area,
                                   Soil_Type)
    
    pseudo_train_input <- pseudo_train %>% 
      tbl_df() 
    pseudo_test_input <- pseudo_test %>% 
      tbl_df()
    
    model_knn <-knn(train=pseudo_train_input, test=pseudo_test_input, cl=pseudo_train$Cover_Type, k=i)
    
    pseudo_test <- pseudo_test %>%
      mutate(
        guess = model_knn)
    pseudo_test <- pseudo_test %>% mutate(correct=ifelse(Cover_Type==guess,1,0))
    
    scores[j] <- mean(pseudo_test$correct)
  }
  k_values[i] <-mean(scores)
  if(i %% 1 == 0){
    print(i)
  }
}

max(k_values)
k_star <- match(max(k_values),k_values)

# Instead, k=450 will be the arbitrary K-star value, and cross validation 
# will be done for this instead.
scores <- as.vector(1:n_folds)
for(j in 1:n_folds)
  {
  
  
  pseudo_train <- train %>%
    filter(fold != j) 
  pseudo_test <- train %>%
    filter(fold == j) 
  
  pseudo_train_input <- pseudo_train %>% 
    tbl_df() %>% select(Elevation,Horizontal_Distance_To_Roadways,
                        Distance_To_Hydrology,Wilderness_Area,
                        Soil_Type)
  pseudo_test_input <- pseudo_test %>% 
    tbl_df()%>% select(Elevation,Horizontal_Distance_To_Roadways,
                       Distance_To_Hydrology,Wilderness_Area,
                       Soil_Type)
  
  model_knn <-knn(train=pseudo_train_input, test=pseudo_test_input, cl=pseudo_train$Cover_Type, k=450)
  
  pseudo_test <- pseudo_test %>%
    mutate(
      guess = model_knn)
  pseudo_test <- pseudo_test %>% mutate(correct=ifelse(Cover_Type==guess,1,0))
  
  scores[j] <- mean(pseudo_test$correct)
}

# Cross Validation Estimate
mean(scores)

k_star <- 450





# 5. Create Submission ----------------------------------------------------
train_input <- train %>% 
  tbl_df() %>% select(Elevation,Horizontal_Distance_To_Roadways,
                      Distance_To_Hydrology,Wilderness_Area,
                      Soil_Type)
test_input <- test %>% 
  tbl_df()%>% select(Elevation,Horizontal_Distance_To_Roadways,
                     Distance_To_Hydrology,Wilderness_Area,
                     Soil_Type)
# This with take a bit, because test has 565,892 entries.
model_knn <-knn(train=train_input, test=test_input, cl=classifications, k=k_star)
test <- test %>%
  mutate(
    Cover_type = model_knn
  )

predictions <- test %>% select(Id,Cover_type )
write.csv(predictions, "submission.csv")

# I don't know if it's a Windows thing, but for write.csv always adds and extra
# row value column, so I manually deleted this for submission to Kaggle.

# This model doesn't do the greatest on Kaggle (.34885). This may be do to the
# arbitrary K-star value chosen. Had I actually been able to determine the k-star
# value, I would expect this score to be higher. On the plus side, it is higher
# than the CV score for linear regression, ridge regression, or LASSO (See Extra
# Section.)







# 6. Extras ---------------------------------------------------------------

# Extra EDA
ggplot(train,aes(x=Cover_Type,y=Slope, group=Cover_Type))+ 
  geom_boxplot()


ggplot(train,aes(x=Cover_Type,y=Aspect, group=Cover_Type))+ 
  geom_boxplot()

# Decent-ish
ggplot(train,aes(x=Cover_Type,y=train$Horizontal_Distance_To_Fire_Points, group=Cover_Type))+ 
  geom_boxplot()

# Decent-ish
ggplot(train,aes(x=Cover_Type,y=train$Hillshade_3pm, group=Cover_Type))+ 
  geom_boxplot()

ggplot(train,aes(x=Cover_Type,y=Avg_Hillshade, group=Cover_Type))+ 
  geom_boxplot()




# Models
model_lm <- lm(Cover_Type~Elevation +
                 Soil_Type+
                 Wilderness_Area+
                 Distance_To_Hydrology+
                 Horizontal_Distance_To_Roadways, data=train)

model_formula <- as.formula(Cover_Type~Elevation +
                              Soil_Type+
                              Wilderness_Area+
                              Distance_To_Hydrology+
                              Horizontal_Distance_To_Roadways)
X <- model.matrix(model_formula, data = train)[, -1]
y <- train$Cover_Type

lambda_values <- 10^seq(from = 2, to = 4, length = 2500)

# Ridge Regression
model_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_values)

# LASSO
model_LASSO <- glmnet(X, y, alpha = 1, lambda = lambda_values)






# Extra CV

#Ridge Regression
cvfit_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = 10)

lambda_star_ridge <- cvfit_ridge %>%
  glance() %>%
  .[["lambda.min"]]

#LASSO
cvfit_LASSO <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = 10)

lambda_star_LASSO <- cvfit_LASSO %>%
  glance() %>%
  .[["lambda.min"]]


# Calculate MSE Values
n_folds <- 10
train <- train %>%
  sample_frac(1) %>%
  mutate(fold = rep(1:n_folds, length=n())) %>%
  arrange(fold)

# For all n_folds folds, save 3 MSE's from the 3 methods here. For each fold,
# we will append/bind the results for each fold.
results_all_folds <- NULL
scores <- NULL

for(i in 1:n_folds){
  # FILL IN BELOW:
  pseudo_test <- train %>% 
    filter(fold==i)
  
  X_new <- model.matrix(model_formula, data = pseudo_test)[, -1]
  
  # Get predictions for all three methods.
  lm_predictions <- model_lm %>% predict(newdata=pseudo_test)
  lm_predictions <- as.integer(lm_predictions)
  ridge_predictions <- as.integer(predict(model_ridge,newx=X_new, s=lambda_star_ridge))
  LASSO_predictions <- as.integer(predict(model_LASSO,newx = X_new, s=lambda_star_LASSO))
  
  # Compute MSE for each method and save them (by appending/binding to
  # results_all_folds)
  results <- 
    # Create data frame of y=SalePrice and predictions from all three methods
    data_frame(
      Cover_Type = pseudo_test$Cover_Type,
      lm = as.vector(lm_predictions),
      ridge = as.vector(ridge_predictions),
      LASSO = as.vector(LASSO_predictions)
    ) 
  results <- results %>% 
    mutate(correct_lm=ifelse(Cover_Type==lm,1,0))
  results <- results %>% 
    mutate(correct_ridge=ifelse(Cover_Type==ridge,1,0))
  results <- results %>% 
    mutate(correct_LASSO=ifelse(Cover_Type==LASSO,1,0))
  
  scores <- data.frame(
    score_lm = mean(results$correct_lm),
    score_ridge = mean(results$correct_ridge),
    score_LASSO = mean(results$correct_LASSO)) %>% 
    # Include fold i variable and append/bind to results_all_folds
    mutate(fold=i) %>% 
    bind_rows(scores)
}

  scores %>% summary()
  
